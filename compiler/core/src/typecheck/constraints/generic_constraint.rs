use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{AnyConstraintTrace, Constraint, ConstraintKind, RunResult, Solver},
        instantiate::InstantiateCtx,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum GenericConstraintMode {
    DefinitionOnly,
    SourceOnly,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericConstraint {
    pub inner: Box<dyn Constraint>,
    pub mode: GenericConstraintMode,
}

impl GenericConstraint {
    pub fn new(inner: Box<dyn Constraint>, mode: GenericConstraintMode) -> Self {
        GenericConstraint { inner, mode }
    }
}

#[typetag::serde]
impl Constraint for GenericConstraint {
    fn kind(&self) -> ConstraintKind {
        self.inner.kind()
    }

    fn node(&self) -> Node {
        self.inner.node()
    }

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace> {
        self.inner.traces_mut()
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        match self.mode {
            GenericConstraintMode::DefinitionOnly => None,
            GenericConstraintMode::SourceOnly => self.inner.instantiate(db, solver, ctx),
        }
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        match self.mode {
            GenericConstraintMode::DefinitionOnly => self.inner.run(db, solver),
            GenericConstraintMode::SourceOnly => RunResult::None,
        }
    }
}
