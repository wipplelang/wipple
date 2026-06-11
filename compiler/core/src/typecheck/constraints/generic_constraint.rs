use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{AnyConstraintTrace, Constraint, RunResult, Solver},
        instantiate::InstantiateCtx,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenericConstraint(pub Box<dyn Constraint>);

#[typetag::serde]
impl Constraint for GenericConstraint {
    fn node(&self) -> Node {
        self.0.node()
    }

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace> {
        self.0.traces_mut()
    }

    fn instantiate(
        &self,
        _db: &mut Db,
        _solver: &mut Solver,
        _ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        None
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        self.0.run(db, solver)
    }
}
