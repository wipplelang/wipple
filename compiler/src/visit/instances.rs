use crate::{
    database::{Db, Fact, NodeRef, Render},
    typecheck::{
        Bound, BoundConstraint, Instance, Instantation, InstantiateConstraint, InstantiateContext,
        Replacements, Solver, Substitutions, TypeConstraint, Typed, group_instances,
    },
    visit::TraitDefinition,
};
use itertools::Itertools;
use std::any::TypeId;

#[derive(Debug, Clone)]
pub struct OverlappingInstances(pub Vec<NodeRef>);

impl Fact for OverlappingInstances {}

impl Render for OverlappingInstances {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "has {} overlapping instances", self.0.len())
    }
}

impl Db {
    pub fn check_for_overlapping_instances(
        &mut self,
        trait_node: &NodeRef,
        instances: impl IntoIterator<Item = Instance>,
    ) {
        let mut solver = Solver::new();
        let instance_groups = group_instances(instances)
            .map(|instances| {
                instances
                    .into_iter()
                    .map(|mut instance| {
                        // Instantiate the *instance's own* parameters
                        let replacements = Replacements::new();
                        let substitutions = Substitutions::new();
                        solver.insert_constraint(InstantiateConstraint::new(Instantation {
                            source_node: instance.node.clone(),
                            definition: instance.node.clone(),
                            replacements: replacements.clone(),
                            substitutions: substitutions.clone(),
                        }));

                        // Instantiate the substitutions for the *trait*

                        let mut ctx = InstantiateContext {
                            db: self,
                            definition: instance.node.clone(),
                            source_node: instance.node.clone(),
                            replacements,
                            substitutions,
                        };

                        instance.substitutions =
                            ctx.instantiate_substitutions(&instance.substitutions);

                        instance
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        solver.run_pass_until(self, Some(TypeId::of::<BoundConstraint>()));

        for instances in instance_groups {
            let mut overlapping = Vec::new();
            for (left_instance, right_instance) in instances.into_iter().tuple_combinations() {
                let mut copy = Solver::new();
                copy.groups = solver.groups.clone();

                copy.as_unify_ctx().unify_substitutions(
                    None,
                    &left_instance.substitutions,
                    &right_instance.substitutions,
                );

                if !copy.error {
                    if !overlapping.contains(&left_instance.node) {
                        overlapping.push(left_instance.node.clone());
                    }

                    if !overlapping.contains(&right_instance.node) {
                        overlapping.push(right_instance.node.clone());
                    }
                }
            }

            if !overlapping.is_empty() {
                self.insert(trait_node, OverlappingInstances(overlapping));
            }
        }
    }

    pub fn run_mismatched_trait(
        &mut self,
        trait_definition: &TraitDefinition,
        mut filter: impl FnMut(&mut Self, &NodeRef) -> bool,
    ) -> Vec<Solver> {
        let mut solvers = Vec::new();
        for (node, Typed { group }) in self.iter::<Typed>().collect::<Vec<_>>() {
            let Some(group) = group else {
                continue;
            };

            if !filter(self, &node) || group.types.len() <= 1 {
                continue;
            }

            for permutations in group.types.clone().into_iter().permutations(2) {
                let mut permutations = permutations.into_iter();
                let left = permutations.next().unwrap();
                let right = permutations.next().unwrap();

                let substitutions = Substitutions::from_iter([
                    (trait_definition.parameters[0].clone(), node.clone().into()),
                    (trait_definition.parameters[1].clone(), right.into()),
                ]);

                let mut solver = Solver::new();

                solver.insert_constraints([
                    TypeConstraint::new(node.clone(), left),
                    BoundConstraint::new(
                        node.clone(),
                        Bound {
                            source_node: node.clone(),
                            bound_node: node.clone(),
                            trait_node: trait_definition.node.clone(),
                            substitutions,
                            optional: true,
                        },
                    ),
                ]);

                solver.run(self);

                solvers.push(solver);
            }
        }

        solvers
    }
}
