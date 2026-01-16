use crate::{
    database::NodeRef,
    typecheck::{
        Bound, Bounds, BoundsItem, BoundsItemInstance, Constraint, ConstraintCtx, ConstraintInfo,
        ConstraintResult, InferredParameter, Instance, Instances, Instantation,
        InstantiateConstraint, InstantiateContext, Replacements, Solver, Substitutions,
    },
};
use std::any::TypeId;

#[derive(Debug, Clone)]
pub struct BoundConstraint {
    info: ConstraintInfo,
    pub bound: Bound,
}

impl BoundConstraint {
    pub fn new(node: NodeRef, bound: Bound) -> Box<dyn Constraint> {
        Box::new(BoundConstraint {
            info: ConstraintInfo::new(node).instance(bound.to_instance()),
            bound,
        })
    }
}

impl Constraint for BoundConstraint {
    fn info(&self) -> &ConstraintInfo {
        &self.info
    }

    fn info_mut(&mut self) -> &mut ConstraintInfo {
        &mut self.info
    }

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint> {
        Box::new(BoundConstraint {
            info: ConstraintInfo::new(self.info.node.clone()),
            bound: Bound {
                source_node: ctx.source_node.clone(),
                trait_node: self.bound.trait_node.clone(),
                substitutions: ctx.instantiate_substitutions(&self.bound.substitutions),
                optional: self.bound.optional,
            },
        })
    }

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult {
        // These are for the *trait's* parameters
        let (bound_inferred, bound_substitutions): (Substitutions, Substitutions) = self
            .bound
            .substitutions
            .entries()
            .partition(|(parameter, _)| {
                // NOTE: No need to instantiate the substitution here; the bound
                // has already been instantiated
                ctx.db.contains::<InferredParameter>(parameter)
            });

        let Instances(instances) = ctx
            .db
            .get::<Instances>(&self.bound.trait_node)
            .unwrap_or_default();

        let instance_groups = [(ctx.implied_instances.to_vec(), true)]
            .into_iter()
            .chain(group_instances(instances))
            .collect::<Vec<_>>();

        let instance_groups_count = instance_groups.len();

        let mut candidates = Vec::new();
        for (index, (instances, keep_generic)) in instance_groups.into_iter().enumerate() {
            let last_instance_set = index + 1 == instance_groups_count;

            for instance in instances {
                if instance.trait_node != self.bound.trait_node {
                    continue;
                }

                let mut copy = Solver::new();
                copy.groups = ctx.groups.clone();
                copy.implied_instances = ctx.implied_instances.to_vec();

                // These are for the *instance's own* parameters, not the trait
                // parameters like with the bound
                let replacements = Replacements::new();
                let substitutions = Substitutions::new();
                copy.insert_constraint(InstantiateConstraint::new(Instantation {
                    source_node: self.bound.source_node.clone(),
                    definition: instance.node.clone(),
                    replacements: replacements.clone(),
                    substitutions: substitutions.clone(),
                }));

                // Run the solver (excluding bounds) to populate `replacements`
                copy.run_pass_until(ctx.db, Some(TypeId::of::<BoundConstraint>()));

                // These are for the *trait's* parameters
                let instance_substitutions = Substitutions::new();
                let instance_inferred = Substitutions::new();
                for (parameter, mut substitution) in instance.substitutions.entries() {
                    if !keep_generic {
                        let mut ctx = InstantiateContext {
                            db: ctx.db,
                            definition: instance.node.clone(),
                            source_node: self.info.node.clone(),
                            replacements: replacements.clone(),
                            substitutions: substitutions.clone(),
                        };

                        substitution = ctx.instantiate_type(&substitution);
                    }

                    if ctx.db.contains::<InferredParameter>(&parameter) {
                        instance_inferred.insert(parameter.clone(), substitution);
                    } else {
                        instance_substitutions.insert(parameter.clone(), substitution);
                    }
                }

                copy.as_unify_ctx().unify_substitutions(
                    None,
                    &instance_substitutions,
                    &bound_substitutions,
                );

                if !copy.error {
                    copy.as_unify_ctx().unify_substitutions(
                        None,
                        &instance_inferred,
                        &bound_inferred,
                    );

                    candidates.push((copy, instance.node, instance.error));
                }
            }

            let resolved_substitutions = bound_substitutions
                .entries()
                .chain(bound_inferred.entries())
                .collect::<Substitutions>();

            let resolved_bound = Bound {
                substitutions: resolved_substitutions,
                ..self.bound.clone()
            };

            // Allow multiple candidates (picking the first) if considering implied
            // instances
            let has_candidate = if keep_generic {
                !candidates.is_empty()
            } else {
                candidates.len() == 1
            };

            if has_candidate {
                let (copy, node, error) = candidates.into_iter().next().unwrap();
                *ctx.groups = copy.groups.clone();

                // Don't indicate a resolved instance if this instance is implied
                // (suppresses custom `[error]` messages)
                let is_implied_instance = ctx
                    .implied_instances
                    .iter()
                    .any(|existing| existing.node == node);

                if !is_implied_instance && !keep_generic {
                    ctx.apply_substitutions(&resolved_bound.substitutions);

                    ctx.db.with_fact(&self.bound.source_node, |Bounds(bounds)| {
                        bounds.push(BoundsItem {
                            bound: resolved_bound,
                            instance: Some(BoundsItemInstance { node, error }),
                        });
                    });
                }

                return ConstraintResult::Enqueue(copy.into_constraints().collect());
            } else if candidates.len() > 1 {
                return ConstraintResult::Enqueue(vec![Box::new(self.clone())]); // ambiguous; try again
            }

            if last_instance_set && !self.bound.optional {
                ctx.apply_substitutions(&resolved_bound.substitutions);

                ctx.db.with_fact(&self.bound.source_node, |Bounds(bounds)| {
                    bounds.push(BoundsItem {
                        bound: resolved_bound,
                        instance: None,
                    });
                });
            }
        }

        ConstraintResult::None
    }
}

pub fn group_instances(
    instances: impl IntoIterator<Item = Instance>,
) -> impl Iterator<Item = (Vec<Instance>, bool)> {
    let mut regular_instances = Vec::new();
    let mut error_instances = Vec::new();
    let mut default_instances = Vec::new();
    let mut default_error_instances = Vec::new();
    for instance in instances {
        match (instance.error, instance.default) {
            (false, false) => regular_instances.push(instance),
            (true, false) => error_instances.push(instance),
            (false, true) => default_instances.push(instance),
            (true, true) => default_error_instances.push(instance),
        }
    }

    [
        (regular_instances, false),
        (error_instances, false),
        (default_instances, false),
        (default_error_instances, false),
    ]
    .into_iter()
}
