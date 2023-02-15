use crate::{
    analysis::ast::{
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

impl TupleType {
    pub fn span(&self) -> Span {
        let first_ty_span = match self.tys.first().unwrap() {
            Ok(ty) => ty.span(),
            Err(error) => error.span,
        };

        if self.tys.len() == 1 {
            Span::join(first_ty_span, self.comma_span)
        } else {
            let last_ty_span = match self.tys.last().unwrap() {
                Ok(ty) => ty.span(),
                Err(error) => error.span,
            };

            Span::join(first_ty_span, last_ty_span)
        }
    }
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
