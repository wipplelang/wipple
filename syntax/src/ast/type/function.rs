use crate::{
    ast::{
        r#type::{Type, TypeSyntaxContext},
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        TypeSyntax,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct FunctionType<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub input: Result<Box<Type<D>>, SyntaxError<D>>,
    pub output: Result<Box<Type<D>>, SyntaxError<D>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for FunctionType<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FunctionType {
            span: Default::default(),
            arrow_span: Default::default(),
            input: arbitrary::Arbitrary::arbitrary(u)?,
            output: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> FunctionType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct FunctionTypeSyntax;

impl<D: Driver> Syntax<D> for FunctionTypeSyntax {
    type Context = TypeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (lhs_span, lhs_exprs), arrow_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let input = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let output = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(FunctionType {
                    span,
                    arrow_span,
                    input: input.map(Box::new),
                    output: output.map(Box::new),
                }
                .into())
            },
        ))
    }
}
