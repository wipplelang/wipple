use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, RunResult, Solver},
        instantiate::InstantiateCtx,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupConstraint {
    pub node: Node,
    pub other: Node,
}

impl GroupConstraint {
    pub fn new(node: Node, other: Node) -> Self {
        GroupConstraint { node, other }
    }
}

#[typetag::serde]
impl Constraint for GroupConstraint {
    fn node(&self) -> Node {
        self.node
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        let node = ctx.instantiate_node(db, solver, self.node);
        let other = ctx.instantiate_node(db, solver, self.other);
        Some(Box::new(GroupConstraint::new(node, other)))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_node(db, self.node, self.other, Some(self.clone()));

        RunResult::None
    }
}
