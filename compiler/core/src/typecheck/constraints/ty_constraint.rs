use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{
            AnyConstraintTrace, Constraint, ConstraintTrace, RunResult, Solver,
            group_constraint::GroupConstraint,
        },
        instantiate::InstantiateCtx,
        ty::{ConstructedTy, Ty},
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyConstraint {
    pub node: Node,
    pub ty: ConstructedTy,
    pub traces: Vec<AnyConstraintTrace>,
}

impl TyConstraint {
    pub fn new(node: Node, ty: ConstructedTy) -> Self {
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
        let ty = ctx.instantiate_constructed_ty(db, solver, &self.ty);
        let traces = ctx.instantiate_traces(db, solver, &self.traces);

        match ty {
            Ty::Node(other) => Some(Box::new(GroupConstraint {
                traces,
                ..GroupConstraint::new(node, other)
            })),
            Ty::Constructed(ty) => Some(Box::new(TyConstraint {
                traces,
                ..TyConstraint::new(node, ty)
            })),
        }
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_ty(db, self.node, self.ty.clone());
        RunResult::None
    }
}
