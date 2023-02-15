use crate::{analysis::ast::{
    expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
    statement::StatementSyntaxContext,
    syntax::{Syntax, SyntaxContext, SyntaxRules},
    Statement, StatementAttributes,
}, parse::Span};

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub attributes: StatementAttributes,
}

impl ExpressionStatement {
    pub fn span(&self) -> Span {
        self.expression.span()
    }
}

impl From<Expression> for Statement {
    fn from(expression: Expression) -> Self {
        ExpressionStatement {
            expression,
            attributes: Default::default(), // TODO: Attributes on expressions?
        }
        .into()
    }
}

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
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
