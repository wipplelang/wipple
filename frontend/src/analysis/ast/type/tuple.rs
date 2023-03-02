use crate::{
    analysis::ast::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse::SpanList,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleType {
    pub span: SpanList,
    pub comma_span: SpanList,
    pub tys: Vec<Result<Type, SyntaxError>>,
}

impl TupleType {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct TupleTypeSyntax;

impl Syntax for TupleTypeSyntax {
    type Context = TypeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            ",",
            OperatorAssociativity::Variadic,
            |context, span, (_span, exprs), comma_span, (_unused_span, unused_exprs), scope| async move {
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
                    span,
                    comma_span,
                    tys,
                }
                .into())
            },
        ))
    }
}
