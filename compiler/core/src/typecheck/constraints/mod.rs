pub mod bound_constraint;
pub mod default_constraint;
pub mod generic_constraint;
pub mod group_constraint;
pub mod instantiate_constraint;
pub mod ty_constraint;

use crate::{
    db::{Db, Node},
    render::Render,
    typecheck::{
        constraints::{
            bound_constraint::BoundConstraint, default_constraint::DefaultConstraint,
            generic_constraint::GenericConstraint, group_constraint::GroupConstraint,
            instantiate_constraint::InstantiateConstraint, ty_constraint::TyConstraint,
        },
        instantiate::InstantiateCtx,
        solver::Solver,
    },
};
use dyn_clone::DynClone;
use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt::Debug,
};

pub enum RunResult {
    None,
    Insert(Vec<Box<dyn Constraint>>),
    Enqueue(Vec<Box<dyn Constraint>>),
}

#[typetag::serde]
pub trait Constraint: Debug + DynClone + Any + Send + Sync {
    fn node(&self) -> Node;

    fn trace(&self) -> Option<Box<dyn ConstraintTrace>> {
        None
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>>;

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult;
}

dyn_clone::clone_trait_object!(Constraint);

impl dyn Constraint {
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }
}

#[typetag::serde]
pub trait ConstraintTrace: Debug + DynClone + Any + Send + Sync + Render {
    fn nodes_mut(&mut self) -> Vec<&mut Node>;

    fn nodes(&self, db: &Db) -> Vec<Node>;

    fn allow_hidden_nodes(&self) -> bool {
        true
    }

    fn source_node_mut(&mut self) -> Option<&mut Node> {
        None
    }

    fn contains(&mut self, db: &Db, nodes: &BTreeSet<Node>) -> bool {
        BTreeSet::from_iter(self.nodes(db))
            .intersection(nodes)
            .next()
            .is_some()
    }
}

dyn_clone::clone_trait_object!(ConstraintTrace);

static CONSTRAINT_ORDER: &[TypeId] = &[
    TypeId::of::<InstantiateConstraint>(),
    TypeId::of::<GroupConstraint>(),
    TypeId::of::<TyConstraint>(),
    TypeId::of::<BoundConstraint>(),
];

#[derive(Debug, Default)]
pub struct Constraints {
    constraints: BTreeMap<TypeId, VecDeque<Box<dyn Constraint>>>,
    default_constraints: Vec<Box<dyn Constraint>>,
}

impl Constraints {
    pub fn insert(&mut self, constraint: Box<dyn Constraint>) {
        let type_id = if let Some(constraint) = constraint.downcast_ref::<GenericConstraint>() {
            constraint.0.as_ref().type_id()
        } else {
            constraint.as_ref().type_id()
        };

        if type_id == TypeId::of::<DefaultConstraint>() {
            self.default_constraints.push(constraint);
        } else {
            assert!(
                CONSTRAINT_ORDER.contains(&type_id),
                "unsupported constraint: {constraint:?}"
            );

            self.constraints
                .entry(type_id)
                .or_default()
                .push_back(constraint);
        }
    }

    pub fn extend(&mut self, constraints: impl IntoIterator<Item = Box<dyn Constraint>>) {
        for constraint in constraints {
            self.insert(constraint);
        }
    }

    pub fn run_until(&mut self, db: &mut Db, solver: &mut Solver, stop: Option<TypeId>) {
        let mut requeued_constraints = Vec::new();
        loop {
            let Some((order, constraint)) = self.dequeue(stop) else {
                break;
            };

            if solver.trace
                && let Some(trace) = constraint.trace()
            {
                db.traces.push((order, trace));
            }

            match constraint.run(db, solver) {
                RunResult::None => {}
                RunResult::Insert(constraints) => {
                    self.extend(constraints);
                }
                RunResult::Enqueue(constraints) => {
                    requeued_constraints.extend(constraints);
                }
            }
        }

        self.extend(requeued_constraints);
    }

    pub fn run_defaults(&mut self, db: &mut Db, solver: &mut Solver) {
        for constraint in self.default_constraints.drain(..) {
            constraint.run(db, solver);
        }
    }

    fn dequeue(&mut self, stop: Option<TypeId>) -> Option<(usize, Box<dyn Constraint>)> {
        for (order, &type_id) in CONSTRAINT_ORDER.iter().enumerate() {
            if stop.is_some_and(|stop| type_id == stop) {
                return None;
            }

            if let Some(constraint) = self
                .constraints
                .get_mut(&type_id)
                .and_then(|constraints| constraints.pop_front())
            {
                return Some((order, constraint));
            }
        }

        None
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut dyn Constraint> {
        self.constraints
            .values_mut()
            .flatten()
            .chain(self.default_constraints.iter_mut())
            .map(|constraint| constraint.as_mut())
    }
}

impl IntoIterator for Constraints {
    type Item = Box<dyn Constraint>;
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(
            self.constraints
                .into_values()
                .flatten()
                .chain(self.default_constraints),
        )
    }
}
