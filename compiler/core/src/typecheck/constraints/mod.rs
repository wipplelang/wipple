pub mod bound_constraint;
pub mod default_constraint;
pub mod generic_constraint;
pub mod instantiate_constraint;
pub mod ty_constraint;

use crate::{
    db::{Db, Node},
    render::{Render, RenderCtx},
    traces::TracesEntry,
    typecheck::{
        groups::Typed,
        instantiate::InstantiateCtx,
        solver::Solver,
        ty::{ConstructedTy, Ty},
    },
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::Any,
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt::Debug,
    ops::{Deref, DerefMut},
};

pub enum RunResult {
    None,
    Insert(Vec<Box<dyn Constraint>>),
    Enqueue(Vec<Box<dyn Constraint>>),
}

#[typetag::serde]
pub trait Constraint: Debug + DynClone + Any + Send + Sync {
    fn kind(&self) -> ConstraintKind;

    fn node(&self) -> Node;

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace>;

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

    fn require_consequences(&self) -> bool {
        false
    }

    fn contains(&mut self, db: &Db, nodes: &BTreeSet<Node>) -> bool {
        BTreeSet::from_iter(self.nodes(db))
            .intersection(nodes)
            .next()
            .is_some()
    }
}

dyn_clone::clone_trait_object!(ConstraintTrace);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnyConstraintTrace {
    pub trace: Box<dyn ConstraintTrace>,
    pub from: Vec<usize>,
    pub source_node: Option<Node>,
}

impl AnyConstraintTrace {
    pub fn new(trace: impl ConstraintTrace) -> Self {
        AnyConstraintTrace {
            trace: Box::new(trace),
            from: Vec::new(),
            source_node: None,
        }
    }
}

impl Deref for AnyConstraintTrace {
    type Target = dyn ConstraintTrace;

    fn deref(&self) -> &Self::Target {
        self.trace.as_ref()
    }
}

impl DerefMut for AnyConstraintTrace {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.trace.as_mut()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintConsequence {
    Group(Node),
    Ty(Node, ConstructedTy),
}

impl ConstraintConsequence {
    pub fn node(&self) -> Node {
        match *self {
            ConstraintConsequence::Group(node) => node,
            ConstraintConsequence::Ty(node, _) => node,
        }
    }
}

impl Render for ConstraintConsequence {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        match self {
            ConstraintConsequence::Group(node) => {
                let Some(Typed(Some(group))) = db.get(*node) else {
                    return;
                };

                let nodes = group
                    .nodes
                    .iter()
                    .filter(|&&node| ctx.filter(db, node))
                    .collect::<Vec<_>>();

                if nodes.len() > 1 {
                    ctx.string("This means ");
                    ctx.list("and", |list| {
                        for &node in &group.nodes {
                            if list.filter(db, node) {
                                list.add(move |ctx| ctx.node(node));
                            }
                        }
                    });
                    ctx.string(" must be a ");
                    ctx.ty(db, &Ty::Node(*node), true);
                    ctx.string(".");
                }
            }
            ConstraintConsequence::Ty(node, ty) => {
                ctx.string("This means ");
                ctx.node(*node);
                ctx.string(" is a ");
                ctx.ty(db, &Ty::Constructed(ty.clone()), true);
                ctx.string(".");
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstraintKind {
    Ty,
    Bound,
}

#[derive(Debug, Default)]
pub struct Constraints(BTreeMap<ConstraintKind, VecDeque<Box<dyn Constraint>>>);

impl Constraints {
    pub fn is_empty(&self) -> bool {
        self.0.values().all(|constraints| constraints.is_empty())
    }

    pub fn insert_front(&mut self, constraint: Box<dyn Constraint>) {
        self.insert_inner(constraint, true);
    }

    pub fn insert_back(&mut self, constraint: Box<dyn Constraint>) {
        self.insert_inner(constraint, false);
    }

    fn insert_inner(&mut self, constraint: Box<dyn Constraint>, front: bool) {
        let constraints = self.0.entry(constraint.kind()).or_default();

        if front {
            constraints.push_front(constraint);
        } else {
            constraints.push_back(constraint);
        }
    }

    pub fn extend_front(
        &mut self,
        constraints: impl IntoIterator<Item = Box<dyn Constraint>, IntoIter: DoubleEndedIterator>,
    ) {
        for constraint in constraints.into_iter().rev() {
            self.insert_front(constraint);
        }
    }

    pub fn extend_back(&mut self, constraints: impl IntoIterator<Item = Box<dyn Constraint>>) {
        for constraint in constraints {
            self.insert_back(constraint);
        }
    }

    pub fn run(&mut self, db: &mut Db, solver: &mut Solver, kind: ConstraintKind) {
        let mut requeued_constraints = Vec::new();
        while let Some(mut constraint) = self.0.get_mut(&kind).and_then(|c| c.pop_front()) {
            if solver.trace {
                let traces = constraint.traces_mut();
                let indices = db.traces.len()..(db.traces.len() + traces.len());

                db.traces
                    .extend(traces.iter().cloned().map(TracesEntry::new));

                solver.active_traces = indices;
            }

            match constraint.run(db, solver) {
                RunResult::None => {}
                RunResult::Insert(constraints) => {
                    self.extend_front(constraints);
                }
                RunResult::Enqueue(constraints) => {
                    requeued_constraints.extend(constraints);
                }
            }
        }

        self.extend_back(requeued_constraints);
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut dyn Constraint> {
        self.0
            .values_mut()
            .flatten()
            .map(|constraint| constraint.as_mut())
    }
}

impl IntoIterator for Constraints {
    type Item = Box<dyn Constraint>;
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.0.into_values().flatten())
    }
}
