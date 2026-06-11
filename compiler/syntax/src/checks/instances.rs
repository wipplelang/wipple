use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{any::TypeId, collections::BTreeMap, ops::ControlFlow};
use wipple_core::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        bounds::{Bound, Instance},
        constraints::{
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
    solver: &mut Solver,
    trait_node: Node,
    instances: impl IntoIterator<Item = Instance>,
) {
    let mut instance_groups = group_instances(db, instances);

    for instance in instance_groups.iter_mut().flatten() {
        // Instantiate the *instance's own* parameters

        let substitutions = solver.insert_substitutions(Default::default(), Default::default());

        solver.constraints.insert(Box::new(InstantiateConstraint {
            source_node: instance.node,
            definition: instance.node,
            substitutions,
            traces: Vec::new(),
        }));

        solver.run_pass_until(db, Some(TypeId::of::<BoundConstraint>()));

        // Instantiate the parameters for the *trait*

        let mut ctx = InstantiateCtx {
            definition: instance.node,
            source_node: instance.node,
            substitutions,
        };

        ctx.instantiate_parameters(db, solver, &mut instance.parameters);
    }

    for instances in instance_groups {
        let mut overlapping = Vec::new();
        for (left_instance, right_instance) in instances.into_iter().tuple_combinations() {
            let mut copy = solver.copy();

            copy.unify_parameters(db, &left_instance.parameters, &right_instance.parameters);

            if copy.error {
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
}

pub fn run_mismatched_trait(
    db: &mut Db,
    trait_node: Node,
    trait_definition: &TraitDefinition,
    mut filter: impl FnMut(&Db, Node) -> bool,
    init: impl Fn(&mut Solver),
) -> Vec<Solver> {
    let mut groups = Vec::new();
    db.for_each_fact::<Typed, ()>(&mut |_, node, Typed(group)| {
        let Some(group) = group.as_ref() else {
            return ControlFlow::Continue(());
        };

        if !filter(db, node) || group.tys.len() <= 1 {
            return ControlFlow::Continue(());
        }

        groups.push((node, group.clone()));

        ControlFlow::Continue(())
    });

    let mut solvers = Vec::new();
    for (node, group) in groups {
        for permutations in group.tys.clone().into_iter().permutations(2) {
            let mut permutations = permutations.into_iter();
            let left = permutations.next().unwrap();
            let right = permutations.next().unwrap();

            let mut parameters = BTreeMap::new();
            parameters.insert(trait_definition.parameters[0], Ty::Node(node));
            parameters.insert(trait_definition.parameters[1], Ty::Constructed(right));

            let mut solver = Solver::default();
            init(&mut solver);

            let substitutions = solver.insert_substitutions(Default::default(), parameters);

            solver
                .constraints
                .insert(Box::new(TyConstraint::new(node, left)));

            solver.constraints.insert(Box::new(BoundConstraint::new(
                node,
                Bound {
                    source_node: node,
                    bound_node: node,
                    trait_node,
                    target_node: None,
                    substitutions,
                    is_optional: true,
                },
            )));

            solver.run(db);

            solvers.push(solver);
        }
    }

    solvers
}
