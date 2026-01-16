use crate::{
    database::NodeRef,
    typecheck::{
        Constraint, ConstraintCtx, ConstraintInfo, ConstraintResult, InstantiateContext, Type,
    },
};

#[derive(Debug, Clone)]
pub struct DefaultConstraint {
    info: ConstraintInfo,
    pub ty: Type,
}

impl DefaultConstraint {
    pub fn new(node: NodeRef, ty: impl Into<Type>) -> Box<dyn Constraint> {
        Box::new(DefaultConstraint {
            info: ConstraintInfo::new(node),
            ty: ty.into(),
        })
    }
}

impl Constraint for DefaultConstraint {
    fn info(&self) -> &ConstraintInfo {
        &self.info
    }

    fn info_mut(&mut self) -> &mut ConstraintInfo {
        &mut self.info
    }

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint> {
        let node = ctx.instantiate_node(&self.info.node);
        let ty = ctx.instantiate_type(&self.ty);

        DefaultConstraint::new(node, ty)
    }

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult {
        if let Type::Node(node) = ctx.apply(&Type::Node(self.info.node.clone())) {
            ctx.unify(Some(Box::new(self.clone())), node, self.ty.clone());
        }

        ConstraintResult::None
    }
}
