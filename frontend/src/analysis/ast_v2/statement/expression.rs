use crate::analysis::ast_v2::{
    expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
    statement::StatementSyntaxContext,
    syntax::{Syntax, SyntaxContext, SyntaxRules},
};

pub type ExpressionStatement = Expression;

pub struct ExpressionStatementSyntax;

impl Syntax for ExpressionStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().combine(ExpressionSyntax::rules())
    }
}

impl From<StatementSyntaxContext> for ExpressionSyntaxContext {
    fn from(context: StatementSyntaxContext) -> Self {
        ExpressionSyntaxContext::new(context.ast_builder)
    }
}
