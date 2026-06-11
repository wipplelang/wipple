use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{AnyConstraintTrace, Constraint, ConstraintTrace, RunResult, Solver},
        instantiate::InstantiateCtx,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupConstraint {
    pub node: Node,
    pub other: Node,
    pub traces: Vec<AnyConstraintTrace>,
}

impl GroupConstraint {
    pub fn new(node: Node, other: Node) -> Self {
        GroupConstraint {
            node,
            other,
            traces: Vec::new(),
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.traces.push(AnyConstraintTrace::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for GroupConstraint {
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
        Some(Box::new(GroupConstraint {
            node: ctx.instantiate_node(db, solver, self.node),
            other: ctx.instantiate_node(db, solver, self.other),
            traces: ctx.instantiate_traces(db, solver, &self.traces),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_node(db, self.node, self.other);

        RunResult::None
    }
}
