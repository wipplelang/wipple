use crate::{
    ast::{
        expression::{Expression, ExpressionSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        ExpressionSyntax,
    },
    Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleExpression<D: Driver> {
    pub span: D::Span,
    pub comma_span: D::Span,
    pub exprs: Vec<Result<Expression<D>, SyntaxError<D>>>,
}

impl<D: Driver> TupleExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct TupleExpressionSyntax;

impl<D: Driver> Syntax<D> for TupleExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, span, (_span, exprs), comma_span, (_unused_span, unused_exprs), scope| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                let exprs = stream::iter(exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<ExpressionSyntax>(context.clone(), expr, scope)
                    })
                    .collect()
                    .await;

                Ok(TupleExpression {
                    span,
                    comma_span,
                    exprs,
                }
                .into())
            },
        ))
    }
}
