use crate::{
    analysis::ast::{
        expression::{Expression, ExpressionSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        ExpressionSyntax,
    },
    parse::SpanList,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleExpression {
    pub span: SpanList,
    pub comma_span: SpanList,
    pub exprs: Vec<Result<Expression, SyntaxError>>,
}

impl TupleExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TupleExpressionSyntax;

impl Syntax for TupleExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
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
