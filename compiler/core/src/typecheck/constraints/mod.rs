pub mod bound_constraint;
pub mod default_constraint;
pub mod generic_constraint;
pub mod instantiate_constraint;
pub mod ty_constraint;

use crate::{
    db::{Db, Node},
    render::{Render, RenderCtx},
    typecheck::{bounds::Instance, instantiate::InstantiateCtx, solver::Solver, ty::ConstructedTy},
    visit::definitions::{Defined, InstanceDefinition},
};
use dyn_clone::DynClone;
use serde::{Deserialize, Serialize};
use std::{
    any::Any,
    collections::{BTreeMap, VecDeque},
    fmt::Debug,
};

pub enum RunResult {
    None,
    Insert(Vec<(Node, Box<dyn Constraint>)>),
    Enqueue(Vec<(Node, Box<dyn Constraint>)>),
}

#[typetag::serde]
pub trait Constraint: Debug + DynClone + Any + Send + Sync {
    fn kind(&self) -> ConstraintKind;

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintConsequence {
    Group(Node, Node, bool),
    Ty(Node, ConstructedTy, bool),
    Instance(Instance, bool),
}

impl ConstraintConsequence {
    pub fn relevant_nodes(&self) -> Vec<Node> {
        match self {
            ConstraintConsequence::Group(left, right, _) => vec![*left, *right],
            ConstraintConsequence::Ty(node, _, _) => vec![*node],
            ConstraintConsequence::Instance(instance, _) => [instance.node]
                .into_iter()
                .chain(
                    instance
                        .parameters
                        .iter()
                        .flat_map(|(&node, ty)| [Some(node), ty.node()])
                        .flatten(),
                )
                .collect(),
        }
    }
}

impl Render for ConstraintConsequence {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        match self {
            ConstraintConsequence::Group(left, right, merged) if !*merged => {
                ctx.string("This requires ");
                ctx.node(*left);
                ctx.string(" and ");
                ctx.node(*right);
                ctx.string(" to have the same type.");
            }
            // ConstraintConsequence::Ty(node, ty, merged) if !*merged => {
            //     ctx.string("This means ");
            //     ctx.node(*node);
            //     ctx.string(" is a ");
            //     ctx.ty(db, &Ty::Constructed(ty.clone()), true);
            //     ctx.string(".");
            // }
            ConstraintConsequence::Instance(instance, resolved)
                if !*resolved
                    || db
                        .get(instance.node)
                        .and_then(|Defined(definition)| {
                            definition.downcast_ref::<InstanceDefinition>()
                        })
                        .is_some_and(|definition| definition.error) =>
            {
                ctx.string("This requires ");
                ctx.render(db, instance);
                ctx.string(".");
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstraintKind {
    Ty,
    Bound,
}

#[derive(Debug, Default)]
pub struct Constraints(BTreeMap<ConstraintKind, VecDeque<(Node, Box<dyn Constraint>)>>);

impl Constraints {
    pub fn is_empty(&self) -> bool {
        self.0.values().all(|constraints| constraints.is_empty())
    }

    pub fn insert_front(&mut self, node: Node, constraint: Box<dyn Constraint>) {
        self.insert_inner(node, constraint, true);
    }

    pub fn insert_back(&mut self, node: Node, constraint: Box<dyn Constraint>) {
        self.insert_inner(node, constraint, false);
    }

    fn insert_inner(&mut self, node: Node, constraint: Box<dyn Constraint>, front: bool) {
        let constraints = self.0.entry(constraint.kind()).or_default();

        if front {
            constraints.push_front((node, constraint));
        } else {
            constraints.push_back((node, constraint));
        }
    }

    pub fn extend_front(
        &mut self,
        constraints: impl IntoIterator<
            Item = (Node, Box<dyn Constraint>),
            IntoIter: DoubleEndedIterator,
        >,
    ) {
        for (node, constraint) in constraints.into_iter().rev() {
            self.insert_front(node, constraint);
        }
    }

    pub fn extend_back(
        &mut self,
        constraints: impl IntoIterator<Item = (Node, Box<dyn Constraint>)>,
    ) {
        for (node, constraint) in constraints.into_iter() {
            self.insert_back(node, constraint);
        }
    }

    pub fn run(&mut self, db: &mut Db, solver: &mut Solver, kind: ConstraintKind) {
        let mut requeued_constraints = Vec::new();
        while let Some((node, constraint)) = self.0.get_mut(&kind).and_then(|c| c.pop_front()) {
            if solver.trace {
                solver.tracing_node = Some(node);
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

            if solver.trace {
                solver.tracing_node = None;
            }
        }

        self.extend_back(requeued_constraints);
    }

    pub fn constraints_mut(&mut self) -> impl Iterator<Item = &mut dyn Constraint> {
        self.0
            .values_mut()
            .flatten()
            .map(|(_, constraint)| constraint.as_mut())
    }
}

impl IntoIterator for Constraints {
    type Item = (Node, Box<dyn Constraint>);
    type IntoIter = Box<dyn Iterator<Item = Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.0.into_values().flatten())
    }
}
