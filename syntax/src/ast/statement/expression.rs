use crate::{
    ast::{
        expression::{Expression, ExpressionSyntax, ExpressionSyntaxContext},
        statement::StatementSyntaxContext,
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        Statement, StatementAttributes,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct ExpressionStatement<D: Driver> {
    pub expression: Expression<D>,
    pub attributes: StatementAttributes<D>,
}

impl<D: Driver> ExpressionStatement<D> {
    pub fn span(&self) -> D::Span {
        self.expression.span()
    }
}

impl<D: Driver> From<Expression<D>> for Statement<D> {
    fn from(expression: Expression<D>) -> Self {
        ExpressionStatement {
            expression,
            attributes: Default::default(), // TODO: Attributes on expressions?
        }
        .into()
    }
}

pub struct ExpressionStatementSyntax;

impl<D: Driver> Syntax<D> for ExpressionStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().combine(ExpressionSyntax::rules())
    }
}

impl<D: Driver> From<StatementSyntaxContext<D>> for ExpressionSyntaxContext<D> {
    fn from(context: StatementSyntaxContext<D>) -> Self {
        ExpressionSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
