use crate::{
    analysis::ast::{
        expression::{Expression, ExpressionSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        ExpressionSyntax,
    },
    parse::Span,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleExpression {
    pub comma_span: Span,
    pub exprs: Vec<Result<Expression, SyntaxError>>,
}

impl TupleExpression {
    pub fn span(&self) -> Span {
        let first_expr_span = match self.exprs.first().unwrap() {
            Ok(expr) => expr.span(),
            Err(error) => error.span,
        };

        if self.exprs.len() == 1 {
            Span::join(first_expr_span, self.comma_span)
        } else {
            let last_expr_span = match self.exprs.last().unwrap() {
                Ok(expr) => expr.span(),
                Err(error) => error.span,
            };

            Span::join(first_expr_span, last_expr_span)
        }
    }
}

pub struct TupleExpressionSyntax;

impl Syntax for TupleExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (_span, exprs), operator_span, (_unused_span, unused_exprs), scope| async move {
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
                    comma_span: operator_span,
                    exprs,
                }
                .into())
            },
        ))
    }
}