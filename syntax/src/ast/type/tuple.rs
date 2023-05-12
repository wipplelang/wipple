use crate::{
    ast::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    Driver,
};
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct TupleType<D: Driver> {
    pub span: D::Span,
    pub comma_span: D::Span,
    pub tys: Vec<Result<Type<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TupleType<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TupleType {
            span: Default::default(),
            comma_span: Default::default(),
            tys: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> TupleType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct TupleTypeSyntax;

impl<D: Driver> Syntax<D> for TupleTypeSyntax {
    type Context = TypeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
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
