use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, ConstraintKind, RunResult, Solver},
        instantiate::InstantiateCtx,
        ty::Ty,
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyConstraint {
    pub node: Node,
    pub ty: Ty,
}

impl TyConstraint {
    pub fn new(node: Node, ty: Ty) -> Self {
        TyConstraint { node, ty }
    }
}

#[typetag::serde]
impl Constraint for TyConstraint {
    fn kind(&self) -> ConstraintKind {
        ConstraintKind::Ty
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
        }))
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify(db, self.node, &self.ty, || {});
        RunResult::None
    }
}
