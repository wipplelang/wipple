use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, ops::ControlFlow};
use wipple_core::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        bounds::{Bound, Instance},
        constraints::{
            ConstraintKind,
            bound_constraint::{BoundConstraint, group_instances},
            instantiate_constraint::InstantiateConstraint,
            ty_constraint::TyConstraint,
        },
        groups::Typed,
        instantiate::InstantiateCtx,
        solver::Solver,
        ty::Ty,
    },
    visit::definitions::TraitDefinition,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverlappingInstances(pub Vec<Node>);

#[typetag::serde]
impl Fact for OverlappingInstances {}

impl Render for OverlappingInstances {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.string(format!("has {} overlapping instances", self.0.len()))
    }
}

pub fn check_for_overlapping_instances(
    db: &mut Db,
    solver: &Solver,
    trait_node: Node,
    instances: impl IntoIterator<Item = Instance>,
) {
    let last_node = db.last_node();

    for instances in group_instances(db, instances) {
        let mut overlapping = Vec::new();
        for (mut left_instance, mut right_instance) in instances.into_iter().tuple_combinations() {
            let mut copy = solver.clone_without_constraints(None);

            for instance in [&mut left_instance, &mut right_instance] {
                // Instantiate the *instance's own* parameters

                let substitutions =
                    copy.insert_substitutions(Default::default(), Default::default());

                copy.constraints.insert_front(
                    instance.node,
                    Box::new(InstantiateConstraint::new(
                        instance.node,
                        instance.node,
                        substitutions,
                    )),
                );

                copy.run_pass(db, ConstraintKind::Ty);

                // Instantiate the parameters for the *trait*

                let mut ctx = InstantiateCtx {
                    definition: instance.node,
                    source_node: instance.node,
                    bound_path: Vec::new(),
                    substitutions,
                };

                ctx.instantiate_parameters(db, &mut copy, &mut instance.parameters);
            }

            let mut error = false;
            copy.unify_parameters(
                db,
                &left_instance.parameters,
                &right_instance.parameters,
                || error = true,
            );

            if error {
                continue;
            }

            if !overlapping.contains(&left_instance.node) {
                overlapping.push(left_instance.node);
            }

            if !overlapping.contains(&right_instance.node) {
                overlapping.push(right_instance.node);
            }
        }

        if !overlapping.is_empty() {
            db.insert(trait_node, OverlappingInstances(overlapping));
        }
    }

    db.delete_range(last_node..=db.last_node());
}

pub fn run_mismatched_trait(
    db: &mut Db,
    solver: &Solver,
    trait_node: Node,
    trait_definition: &TraitDefinition,
    mut filter: impl FnMut(&Db, Node) -> bool,
) -> Vec<Solver> {
    let mut groups = Vec::new();
    db.for_each_fact::<Typed, ()>(false, &mut |_, node, Typed(group)| {
        let Some(group) = group.as_ref() else {
            return ControlFlow::Continue(());
        };

        if !filter(db, node) || group.tys().count() <= 1 {
            return ControlFlow::Continue(());
        }

        groups.push((node, group.clone()));

        ControlFlow::Continue(())
    });

    let mut solvers = Vec::new();
    for (node, group) in groups {
        for permutations in group.tys().cloned().permutations(2) {
            let mut permutations = permutations.into_iter();
            let left = permutations.next().unwrap();
            let right = permutations.next().unwrap();

            let mut parameters = BTreeMap::new();
            parameters.insert(trait_definition.parameters[0], Ty::Node(node));
            parameters.insert(trait_definition.parameters[1], Ty::Constructed(right));

            let mut solver = solver.clone_without_constraints(None);

            let substitutions = solver.insert_substitutions(Default::default(), parameters);

            solver.constraints.insert_back(
                node,
                Box::new(TyConstraint::new(node, Ty::Constructed(left))),
            );

            solver.constraints.insert_back(
                node,
                Box::new(BoundConstraint::new(
                    node,
                    Bound {
                        source_node: node,
                        bound_path: Vec::new(),
                        bound_node: node,
                        trait_node,
                        substitutions,
                        is_optional: true,
                    },
                )),
            );

            solver.run(db);

            solvers.push(solver);
        }
    }

    solvers
}
