use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, RunResult, Solver},
        instantiate::InstantiateCtx,
        ty::Ty,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultConstraint {
    pub node: Node,
    pub default: Node,
}

impl DefaultConstraint {
    pub fn new(node: Node, default: Node) -> Self {
        DefaultConstraint { node, default }
    }
}

#[typetag::serde]
impl Constraint for DefaultConstraint {
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
        let default = ctx.instantiate_node(db, solver, self.default);
        Some(Box::new(DefaultConstraint::new(node, default)))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        if let Ty::Node(node) = solver.apply_ty(Ty::Node(self.node)) {
            solver.unify_with_node(db, node, self.default, Some(self.clone()));
        }

        RunResult::None
    }
}
