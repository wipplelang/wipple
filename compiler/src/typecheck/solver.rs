use crate::{
    database::{Db, NodeRef},
    typecheck::{Constraint, ConstraintCtx, Constraints, Group, Groups, Instance, UnifyCtx},
};
use std::any::TypeId;

const ITERATION_LIMIT: usize = 32;

#[derive(Debug, Default)]
pub struct Solver {
    pub groups: Groups,
    pub implied_instances: Vec<Instance>,
    pub progress: bool,
    pub error: bool,
    constraints: Constraints,
    iterations: usize,
}

impl Solver {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert_group(&mut self, group: Group) {
        self.as_unify_ctx().insert_group(group);
    }

    pub fn into_sorted_groups<K: Ord>(
        mut self,
        key: impl FnMut(&NodeRef) -> K,
    ) -> impl Iterator<Item = Group> {
        self.as_unify_ctx().apply_all();
        self.groups.into_sorted(key)
    }

    pub fn into_constraints(self) -> impl Iterator<Item = Box<dyn Constraint>> + use<> {
        self.constraints.into_constraints()
    }

    pub fn run(&mut self, db: &mut Db) {
        loop {
            self.progress = false;

            self.run_pass_until(db, None);

            if !self.progress {
                break;
            }
        }

        // Run a final pass
        self.run_pass_until(db, None);
    }

    pub fn run_pass_until(&mut self, db: &mut Db, stop: Option<TypeId>) {
        if self.iterations >= ITERATION_LIMIT {
            panic!("reached iteration limit");
        }

        let mut ctx = ConstraintCtx {
            db,
            implied_instances: &mut self.implied_instances,
            unify_ctx: UnifyCtx {
                groups: &mut self.groups,
                progress: &mut self.progress,
                error: &mut self.error,
            },
        };

        self.constraints.run_until(&mut ctx, stop);

        if !*ctx.progress {
            self.constraints.run_defaults(&mut ctx);
        }

        self.iterations += 1;
    }

    pub fn imply(&mut self, instance: Instance) {
        if self
            .implied_instances
            .iter()
            .any(|existing| existing.node == instance.node)
        {
            return;
        }

        self.implied_instances.push(instance);
    }

    pub fn insert_constraint(&mut self, constraint: Box<dyn Constraint>) {
        self.constraints.insert(constraint);
    }

    pub fn insert_constraints(&mut self, iter: impl IntoIterator<Item = Box<dyn Constraint>>) {
        self.constraints.extend(iter);
    }

    pub fn as_unify_ctx(&mut self) -> UnifyCtx<'_> {
        UnifyCtx {
            groups: &mut self.groups,
            progress: &mut self.progress,
            error: &mut self.error,
        }
    }
}
