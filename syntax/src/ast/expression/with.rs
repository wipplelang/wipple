use crate::{
    ast::{
        format::Format,
        syntax::{Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules},
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WithClause, WithClauseSyntax,
        WithClauseSyntaxContext,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct WithExpression<D: Driver> {
    pub span: D::Span,
    pub when_span: D::Span,
    pub clause: Result<WithClause<D>, SyntaxError<D>>,
    pub body: Result<Box<Expression<D>>, SyntaxError<D>>,
}

impl<D: Driver> WithExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for WithExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "(with {} {})",
            self.clause?.format()?,
            self.body?.format()?,
        ))
    }
}

pub struct WithExpressionSyntax;

impl<D: Driver> Syntax<D> for WithExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "with",
            |context, span, when_span, exprs, scope_set| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`with` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let clause = context
                    .ast_builder
                    .build_expr::<WithClauseSyntax>(
                        WithClauseSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        exprs.next().unwrap(),
                        scope_set.clone(),
                    )
                    .await;

                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        context.clone(),
                        exprs.next().unwrap(),
                        scope_set,
                    )
                    .await;

                Ok(WithExpression {
                    span,
                    when_span,
                    clause,
                    body: body.map(Box::new),
                }
                .into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::WITH]
}
