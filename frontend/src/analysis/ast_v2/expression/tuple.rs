use crate::{
    analysis::ast_v2::{
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
    pub tys: Vec<Result<Expression, SyntaxError>>,
}

pub struct TupleExpressionSyntax;

impl Syntax for TupleExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (_span, exprs), operator_span, (_unused_span, unused_exprs)| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                let tys = stream::iter(exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<ExpressionSyntax>(context.clone(), expr)
                    })
                    .collect()
                    .await;

                Ok(TupleExpression {
                    comma_span: operator_span,
                    tys,
                }
                .into())
            },
        ))
    }
}
