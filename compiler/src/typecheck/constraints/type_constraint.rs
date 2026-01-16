use crate::{
    database::NodeRef,
    typecheck::{
        Constraint, ConstraintCtx, ConstraintInfo, ConstraintResult, ConstructedType,
        GroupConstraint, InstantiateContext, Type,
    },
};

#[derive(Debug, Clone)]
pub struct TypeConstraint {
    info: ConstraintInfo,
    pub ty: ConstructedType,
}

impl TypeConstraint {
    pub fn new(node: NodeRef, ty: ConstructedType) -> Box<dyn Constraint> {
        Box::new(TypeConstraint {
            info: ConstraintInfo::new(node),
            ty,
        })
    }
}

impl Constraint for TypeConstraint {
    fn info(&self) -> &ConstraintInfo {
        &self.info
    }

    fn info_mut(&mut self) -> &mut ConstraintInfo {
        &mut self.info
    }

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint> {
        let node = ctx.instantiate_node(&self.info.node);
        let ty = ctx.instantiate_type(&Type::Constructed(self.ty.clone()));

        match ty {
            Type::Node(other) => GroupConstraint::new(node, other),
            Type::Constructed(ty) => TypeConstraint::new(node, ty),
        }
    }

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult {
        ctx.unify(
            Some(Box::new(self.clone())),
            self.info.node.clone(),
            self.ty.clone(),
        );

        ConstraintResult::None
    }
}
