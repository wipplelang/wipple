use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, ConstraintTrace, RunResult, Solver},
        instantiate::InstantiateCtx,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupConstraint {
    pub node: Node,
    pub other: Node,
    pub trace: Option<Box<dyn ConstraintTrace>>,
}

impl GroupConstraint {
    pub fn new(node: Node, other: Node) -> Self {
        GroupConstraint {
            node,
            other,
            trace: None,
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.trace = Some(Box::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for GroupConstraint {
    fn node(&self) -> Node {
        self.node
    }

    fn trace(&self) -> Option<Box<dyn ConstraintTrace>> {
        self.trace.clone()
    }

    fn instantiate(
        &self,
        db: &mut Db,
        solver: &mut Solver,
        ctx: &mut InstantiateCtx,
    ) -> Option<Box<dyn Constraint>> {
        Some(Box::new(GroupConstraint {
            node: ctx.instantiate_node(db, solver, self.node),
            other: ctx.instantiate_node(db, solver, self.other),
            trace: ctx.instantiate_trace(db, solver, &self.trace),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_node(db, self.node, self.other);

        RunResult::None
    }
}
