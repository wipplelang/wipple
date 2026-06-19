use crate::QueryCtx;
use wipple_core::{
    db::Node,
    facts::{Children, Parent, Syntax},
};
use wipple_syntax::{
    expressions::block_expression::BlockExpression, file::File,
    statements::expression_statement::ExpressionStatement,
};

pub fn unused_block(db: &QueryCtx<'_>, node: Node) -> bool {
    let Some(syntax) = db.get::<Syntax>(node) else {
        return false;
    };

    if db
        .ast(&syntax.0)
        .downcast_ref::<BlockExpression>()
        .is_none()
    {
        return false;
    }

    let Some(Parent(statement)) = db.get(node) else {
        return false;
    };

    let Some(Syntax(statement_syntax)) = db.get(*statement) else {
        return false;
    };

    if db
        .ast(statement_syntax)
        .downcast_ref::<ExpressionStatement>()
        .is_none()
    {
        return false;
    }

    let Some(Parent(statements)) = db.get(*statement) else {
        return false;
    };

    let Some(Syntax(statements_syntax)) = db.get(*statements) else {
        return false;
    };

    let check_last = if db
        .ast(statements_syntax)
        .downcast_ref::<BlockExpression>()
        .is_some()
    {
        true
    } else if db.ast(statements_syntax).downcast_ref::<File>().is_some() {
        false
    } else {
        return false;
    };

    let statement_nodes = db
        .get(*statements)
        .map(|Children(children)| children.as_slice())
        .unwrap_or_default();

    statement_nodes
        .iter()
        .rev()
        .skip(if check_last { 0 } else { 1 })
        .any(|other| *other == *statement)
}
