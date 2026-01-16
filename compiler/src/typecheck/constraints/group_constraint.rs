use crate::{
    database::NodeRef,
    typecheck::{Constraint, ConstraintCtx, ConstraintInfo, ConstraintResult, InstantiateContext},
};

#[derive(Debug, Clone)]
pub struct GroupConstraint {
    info: ConstraintInfo,
    pub other: NodeRef,
}

impl GroupConstraint {
    pub fn new(node: NodeRef, other: NodeRef) -> Box<dyn Constraint> {
        Box::new(Self {
            info: ConstraintInfo::new(node),
            other,
        })
    }
}

impl Constraint for GroupConstraint {
    fn info(&self) -> &ConstraintInfo {
        &self.info
    }

    fn info_mut(&mut self) -> &mut ConstraintInfo {
        &mut self.info
    }

    fn instantiate(&self, ctx: &mut InstantiateContext<'_>) -> Box<dyn Constraint> {
        let node = ctx.instantiate_node(&self.info.node);
        let other = ctx.instantiate_node(&self.other);

        GroupConstraint::new(node, other)
    }

    fn run(&mut self, ctx: &mut ConstraintCtx<'_, '_>) -> ConstraintResult {
        ctx.unify(
            Some(Box::new(self.clone())),
            self.info.node.clone(),
            self.other.clone(),
        );

        ConstraintResult::None
    }
}
