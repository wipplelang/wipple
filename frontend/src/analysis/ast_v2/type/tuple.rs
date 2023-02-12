use crate::{
    analysis::ast_v2::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse::Span,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleType {
    pub comma_span: Span,
    pub tys: Vec<Result<Type, SyntaxError>>,
}

pub struct TupleTypeSyntax;

impl Syntax for TupleTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, (_span, exprs), operator_span, (_unused_span, unused_exprs), scope| async move {
                // HACK: All of the expressions are contained in `lhs`. In the
                // future, handle variadic operators specially.
                assert!(unused_exprs.is_empty());

                let tys = stream::iter(exprs)
                    .then(|expr| {
                        context
                            .ast_builder
                            .build_expr::<TypeSyntax>(context.clone(), expr, scope)
                    })
                    .collect()
                    .await;

                Ok(TupleType {
                    comma_span: operator_span,
                    tys,
                }
                .into())
            },
        ))
    }
}
