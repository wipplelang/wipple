use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{
            Constraint, ConstraintTrace, RunResult, Solver, group_constraint::GroupConstraint,
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
    pub trace: Option<Box<dyn ConstraintTrace>>,
}

impl TyConstraint {
    pub fn new(node: Node, ty: ConstructedTy) -> Self {
        TyConstraint {
            node,
            ty,
            trace: None,
        }
    }

    pub fn with_trace(mut self, trace: impl ConstraintTrace) -> Self {
        self.trace = Some(Box::new(trace));
        self
    }
}

#[typetag::serde]
impl Constraint for TyConstraint {
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
        let node = ctx.instantiate_node(db, solver, self.node);
        let ty = ctx.instantiate_ty(db, solver, &Ty::Constructed(self.ty.clone()));
        let trace = ctx.instantiate_trace(db, solver, &self.trace);

        match ty {
            Ty::Node(other) => Some(Box::new(GroupConstraint {
                trace,
                ..GroupConstraint::new(node, other)
            })),
            Ty::Constructed(ty) => Some(Box::new(TyConstraint {
                trace,
                ..TyConstraint::new(node, ty)
            })),
        }
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_ty(db, self.node, self.ty.clone());
        RunResult::None
    }
}
