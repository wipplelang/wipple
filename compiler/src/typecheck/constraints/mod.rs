mod bound_constraint;
mod default_constraint;
mod group_constraint;
mod instantiate_constraint;
mod type_constraint;

pub use bound_constraint::*;
pub use default_constraint::*;
pub use group_constraint::*;
pub use instantiate_constraint::*;
pub use type_constraint::*;

use crate::{
    database::{Db, NodeRef},
    typecheck::{Instance, InstantiateContext, UnifyCtx},
};
use dyn_clone::DynClone;
use std::{
    any::{Any, TypeId},
    collections::{HashMap, VecDeque},
    fmt::Debug,
    ops::{Deref, DerefMut},
    sync::LazyLock,
};

pub struct ConstraintCtx<'a, 'db> {
    pub db: &'db mut Db,
    pub implied_instances: &'a [Instance],
    pub unify_ctx: UnifyCtx<'a>,
}

impl<'a, 'db> Deref for ConstraintCtx<'a, 'db> {
    type Target = UnifyCtx<'a>;

    fn deref(&self) -> &Self::Target {
        &self.unify_ctx
    }
}

impl<'a, 'db> DerefMut for ConstraintCtx<'a, 'db> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.unify_ctx
    }
}

pub trait Constraint: Any + Debug + DynClone + Send + Sync + 'static {
    fn info(&self) -> &ConstraintInfo;
    fn info_mut(&mut self) -> &mut ConstraintInfo;

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint>;

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult;

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

dyn_clone::clone_trait_object!(Constraint);

impl dyn Constraint {
    pub fn downcast_ref<T: Constraint>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }
}

#[derive(Debug)]
pub enum ConstraintResult {
    None,
    Insert(Vec<Box<dyn Constraint>>),
    Enqueue(Vec<Box<dyn Constraint>>),
}

#[derive(Debug, Clone)]
pub struct ConstraintInfo {
    pub node: NodeRef,
    pub instance: Option<Instance>,
    pub active: bool,
    pub instantiate: bool,
}

impl ConstraintInfo {
    pub fn new(node: NodeRef) -> Self {
        Self {
            node,
            instance: None,
            active: true,
            instantiate: true,
        }
    }

    pub fn instance(mut self, instance: Instance) -> Self {
        self.instance = Some(instance);
        self
    }
}

static CONSTRAINT_ORDER: LazyLock<Vec<TypeId>> = LazyLock::new(|| {
    vec![
        TypeId::of::<GroupConstraint>(),
        TypeId::of::<TypeConstraint>(),
        TypeId::of::<InstantiateConstraint>(),
        TypeId::of::<BoundConstraint>(),
    ]
});

#[derive(Debug, Default)]
pub struct Constraints {
    constraints: HashMap<TypeId, VecDeque<Box<dyn Constraint>>>,
    default_constraints: Vec<Box<dyn Constraint>>,
}

impl Constraints {
    pub fn insert(&mut self, constraint: Box<dyn Constraint>) {
        let key = constraint.as_ref().type_id();

        if key == TypeId::of::<DefaultConstraint>() {
            self.default_constraints.push(constraint);
        } else {
            self.constraints
                .entry(key)
                .or_default()
                .push_back(constraint);
        }
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = Box<dyn Constraint>>) {
        for constraint in iter {
            self.insert(constraint);
        }
    }

    pub fn run_until(&mut self, ctx: &mut ConstraintCtx<'_, '_>, stop: Option<TypeId>) {
        let mut requeued_constraints = Vec::new();
        loop {
            let Some(mut constraint) = self.dequeue(stop) else {
                break;
            };

            if constraint.info().active {
                match constraint.run(ctx) {
                    ConstraintResult::None => {}
                    ConstraintResult::Insert(constraints) => {
                        self.extend(constraints);
                    }
                    ConstraintResult::Enqueue(constraints) => {
                        requeued_constraints.extend(constraints);
                    }
                }
            }
        }

        self.extend(requeued_constraints);
    }

    fn dequeue(&mut self, stop: Option<TypeId>) -> Option<Box<dyn Constraint>> {
        for &key in CONSTRAINT_ORDER.iter() {
            if stop.is_some_and(|stop| key == stop) {
                return None;
            }

            if let Some(constraint) = self
                .constraints
                .get_mut(&key)
                .and_then(|constraints| constraints.pop_front())
            {
                return Some(constraint);
            }
        }

        None
    }

    pub fn run_defaults(&mut self, ctx: &mut ConstraintCtx<'_, '_>) {
        for mut constraint in self.default_constraints.drain(..) {
            constraint.run(ctx);
        }
    }

    pub fn into_constraints(self) -> impl Iterator<Item = Box<dyn Constraint>> {
        self.constraints
            .into_values()
            .flatten()
            .chain(self.default_constraints)
    }
}
