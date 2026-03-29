use crate::{
    database::{NodeRef, Parent},
    nodes::{BlockExpressionNode, ExpressionStatementNode, FileNode},
    queries::QueryCtx,
};

pub fn unused_block(ctx: &QueryCtx<'_>, f: &mut dyn FnMut(NodeRef)) {
    if ctx.node.downcast_ref::<BlockExpressionNode>().is_none() {
        return;
    }

    let Some(Parent(statement)) = ctx.db.get(&ctx.node) else {
        return;
    };

    if statement
        .downcast_ref::<ExpressionStatementNode>()
        .is_none()
    {
        return;
    };

    let Some(Parent(statements)) = ctx.db.get(&statement) else {
        return;
    };

    let (statements, check_last) =
        if let Some(parent) = statements.downcast_ref::<BlockExpressionNode>() {
            (&parent.statements, false)
        } else if let Some(parent) = statements.downcast_ref::<FileNode>() {
            (&parent.statements, true)
        } else {
            return;
        };

    if statements
        .iter()
        .rev()
        .skip(if check_last { 0 } else { 1 })
        .any(|node| *node == statement)
    {
        f(ctx.node.clone());
    }
}
