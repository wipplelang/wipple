use crate::{
    db::{Db, Node},
    typecheck::{
        constraints::{Constraint, RunResult, Solver, group_constraint::GroupConstraint},
        instantiate::InstantiateCtx,
        ty::{ConstructedTy, Ty},
    },
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyConstraint {
    pub node: Node,
    pub ty: ConstructedTy,
}

impl TyConstraint {
    pub fn new(node: Node, ty: ConstructedTy) -> Self {
        TyConstraint { node, ty }
    }
}

#[typetag::serde]
impl Constraint for TyConstraint {
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
        let ty = ctx.instantiate_ty(db, solver, &Ty::Constructed(self.ty.clone()));

        match ty {
            Ty::Node(other) => Some(Box::new(GroupConstraint::new(node, other))),
            Ty::Constructed(ty) => Some(Box::new(TyConstraint::new(node, ty))),
        }
    }

    fn run(self: Box<Self>, db: &mut Db, solver: &mut Solver) -> RunResult {
        solver.unify_with_ty(db, self.node, self.ty.clone(), Some(self.clone()));
        RunResult::None
    }
}
