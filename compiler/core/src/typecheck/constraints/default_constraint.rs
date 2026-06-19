use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{AnyConstraintTrace, Constraint, ConstraintKind, RunResult, Solver},
        instantiate::InstantiateCtx,
        ty::Ty,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultConstraint {
    pub node: Node,
    pub default: Node,
    pub traces: Vec<AnyConstraintTrace>,
}

impl DefaultConstraint {
    pub fn new(node: Node, default: Node) -> Self {
        DefaultConstraint {
            node,
            default,
            traces: Vec::new(),
        }
    }
}

#[typetag::serde]
impl Constraint for DefaultConstraint {
    fn kind(&self) -> ConstraintKind {
        ConstraintKind::Bound
    }

    fn node(&self) -> Node {
        self.node
    }

    fn traces_mut(&mut self) -> &mut Vec<AnyConstraintTrace> {
        &mut self.traces
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
        if let Ty::Node(node) = solver.apply_ty(db, &Ty::Node(self.node)) {
            solver.unify(db, node, &Ty::Node(self.default), None);
        }

        RunResult::None
    }
}
