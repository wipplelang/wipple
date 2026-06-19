use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{
            AnyConstraintTrace, Constraint, ConstraintKind, ConstraintTrace, RunResult, Solver,
        },
        instantiate::InstantiateCtx,
        ty::Ty,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyConstraint {
    pub node: Node,
    pub ty: Ty,
    pub traces: Vec<AnyConstraintTrace>,
}

impl TyConstraint {
    pub fn new(node: Node, ty: Ty) -> Self {
        TyConstraint {
            node,
            ty,
            traces: Vec::new(),
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.traces.push(AnyConstraintTrace::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for TyConstraint {
    fn kind(&self) -> ConstraintKind {
        ConstraintKind::Ty
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
        Some(Box::new(TyConstraint {
            node: ctx.instantiate_node(db, solver, self.node),
            ty: ctx.instantiate_ty(db, solver, &self.ty),
            traces: ctx.instantiate_traces(db, solver, &self.traces),
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify(db, self.node, &self.ty, None);
        RunResult::None
    }
}
