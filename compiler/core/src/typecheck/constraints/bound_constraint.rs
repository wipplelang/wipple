use crate::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        bounds::{Bound, Bounds, Instance, Instances, ResolvedBound, UnresolvedBound},
        constraints::{
            AnyConstraintTrace, Constraint, ConstraintTrace, RunResult, Solver,
            instantiate_constraint::InstantiateConstraint,
        },
        instantiate::InstantiateCtx,
        ty::Ty,
    },
    visit::definitions::{Defined, InstanceDefinition},
};
use serde::{Deserialize, Serialize};
use std::{any::TypeId, collections::BTreeMap};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferredParameter;

#[typetag::serde]
impl Fact for InferredParameter {}

impl Render for InferredParameter {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string("is inferred type parameter");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsBound;

#[typetag::serde]
impl Fact for IsBound {}

impl Render for IsBound {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BoundConstraint {
    pub node: Node,
    pub bound: Bound,
    pub traces: Vec<AnyConstraintTrace>,
}

impl BoundConstraint {
    pub fn new(node: Node, bound: Bound) -> Self {
        BoundConstraint {
            node,
            bound,
            traces: Vec::new(),
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.traces.push(AnyConstraintTrace::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for BoundConstraint {
    fn node(&self) -> Node {
        self.node
    }

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace> {
        &mut self.traces
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        let node = ctx.instantiate_node(db, solver, self.node);

        let (nodes, mut parameters) = solver.get_substitutions(self.bound.substitutions);

        ctx.instantiate_parameters(db, solver, &mut parameters);

        let substitutions = solver.insert_substitutions(nodes, parameters);

        let bound = Bound {
            source_node: ctx.source_node,
            substitutions,
            ..self.bound
        };

        Some(Box::new(BoundConstraint {
            node,
            bound,
            traces: ctx.instantiate_traces(db, solver, &self.traces),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        let (_, parameters) = solver.get_substitutions(self.bound.substitutions);

        // These are for the *trait's* parameters
        let mut bound_inferred = BTreeMap::new();
        let mut bound_parameters = BTreeMap::new();
        for (parameter, ty) in parameters {
            // NOTE: No need to instantiate the substitution here; the bound
            // has already been instantiated
            if db.contains::<InferredParameter>(parameter) {
                bound_inferred.insert(parameter, ty);
            } else {
                bound_parameters.insert(parameter, ty);
            }
        }

        let Instances(instances) = db.get(self.bound.trait_node).cloned().unwrap_or_default();

        let mut instance_groups = vec![(solver.implied_instances.clone(), true)];
        for group in group_instances(db, instances) {
            instance_groups.push((group, false));
        }

        let instance_group_count = instance_groups.len();

        let resolved_node = db.node();

        let target_node = self.bound.target_node;

        struct Candidate {
            solver: Solver,
            instance: Instance,
            parameters: BTreeMap<Node, Ty>,
        }

        let mut candidates = Vec::new();
        for (index, (instances, keep_generic)) in instance_groups.into_iter().enumerate() {
            let is_last_instance_group = index + 1 == instance_group_count;

            for instance in instances {
                if instance.trait_node != self.bound.trait_node {
                    continue;
                }

                let mut copy = solver.copy();

                // These are for the *instance's own* parameters, not the trait
                // parameters like with the bound
                let substitutions =
                    copy.insert_substitutions(Default::default(), Default::default());

                copy.constraints.insert(Box::new(InstantiateConstraint {
                    source_node: resolved_node,
                    definition: instance.node,
                    substitutions,
                    traces: Vec::new(),
                }));

                // Run the solver (excluding bounds) to populate `replacements`
                copy.run_pass_until(db, Some(TypeId::of::<BoundConstraint>()));

                // Propagate the target node to new bounds
                for constraint in copy.constraints.iter_mut() {
                    let Some(constraint) = constraint.downcast_mut::<BoundConstraint>() else {
                        continue;
                    };

                    constraint
                        .bound
                        .target_node
                        .get_or_insert_with(|| target_node.unwrap_or(self.bound.source_node));
                }

                // These are for the *trait's* parameters
                let mut instance_parameters = BTreeMap::new();
                let mut instance_inferred = BTreeMap::new();
                for (&parameter, substitution) in &instance.parameters {
                    let substitution = if keep_generic {
                        substitution.clone()
                    } else {
                        let mut ctx = InstantiateCtx {
                            definition: instance.node,
                            source_node: resolved_node,
                            substitutions,
                        };

                        ctx.instantiate_ty(db, &mut copy, substitution)
                    };

                    if db.contains::<InferredParameter>(parameter) {
                        instance_inferred.insert(parameter, substitution);
                    } else {
                        instance_parameters.insert(parameter, substitution);
                    }
                }

                copy.error = false;

                copy.unify_parameters(db, &instance_parameters, &bound_parameters);

                if !copy.error {
                    copy.unify_parameters(db, &instance_inferred, &bound_inferred);

                    let (_, parameters) = copy.get_substitutions(substitutions);

                    candidates.push(Candidate {
                        solver: copy,
                        instance: instance.clone(),
                        parameters,
                    });
                }
            }

            let mut resolved_parameters = BTreeMap::new();
            resolved_parameters.extend(bound_parameters.clone());
            resolved_parameters.extend(bound_inferred.clone());

            // Allow multiple candidates (picking the first) if considering implied
            // instances
            let has_candidate = if keep_generic {
                !candidates.is_empty()
            } else {
                candidates.len() == 1
            };

            if has_candidate {
                let candidate = candidates.into_iter().next().unwrap();

                let instance = Instance {
                    parameters: resolved_parameters.clone(),
                    ..candidate.instance.clone()
                };

                let constraints = solver.inherit(candidate.solver);

                // Record the resolved bound on the current source node
                // (necessary for codegen) and the original target node (set
                // above; necessary for error reporting).
                for node in [target_node, Some(self.bound.source_node)] {
                    let Some(node) = node else {
                        continue;
                    };

                    db.get_mut_or_default::<Bounds>(node).0.push((
                        self.bound.bound_node,
                        Ok(ResolvedBound {
                            instance: instance.clone(),
                            instance_parameters: candidate.parameters.clone(),
                            resolved_node,
                        }),
                    ));
                }

                solver.progress = true;

                return RunResult::Enqueue(Vec::from_iter(constraints));
            } else if candidates.len() > 1 {
                return RunResult::Enqueue(vec![self]); // ambiguous; try again
            }

            if is_last_instance_group && !self.bound.is_optional {
                for node in [target_node, Some(self.bound.source_node)] {
                    let Some(node) = node else {
                        continue;
                    };

                    db.get_mut_or_default::<Bounds>(node).0.push((
                        self.bound.bound_node,
                        Err(UnresolvedBound {
                            trait_node: self.bound.trait_node,
                            parameters: resolved_parameters.clone(),
                        }),
                    ));
                }
            }
        }

        RunResult::None
    }
}

pub fn group_instances(
    db: &Db,
    instances: impl IntoIterator<Item = Instance>,
) -> Vec<Vec<Instance>> {
    let mut regular_instances = Vec::new();
    let mut error_instances = Vec::new();
    let mut default_instances = Vec::new();
    let mut default_error_instances = Vec::new();
    for instance in instances {
        let Some(definition) = db
            .get(instance.node)
            .and_then(|Defined(definition)| definition.downcast_ref::<InstanceDefinition>())
        else {
            regular_instances.push(instance);
            continue;
        };

        match (definition.default, definition.error) {
            (false, false) => regular_instances.push(instance),
            (true, false) => default_instances.push(instance),
            (false, true) => error_instances.push(instance),
            (true, true) => default_error_instances.push(instance),
        }
    }

    vec![
        regular_instances,
        error_instances,
        default_instances,
        default_error_instances,
    ]
}
