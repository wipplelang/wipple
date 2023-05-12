use crate::{
    ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        when_arm::WhenArmSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, WhenPattern, WhenPatternSyntax,
        WhenPatternSyntaxContext,
    },
    parse, Driver, File,
};

#[derive(Debug, Clone)]
pub struct FunctionWhenArm<D: Driver> {
    pub span: D::Span,
    pub arrow_span: D::Span,
    pub pattern: Result<WhenPattern<D>, SyntaxError<D>>,
    pub body: Result<Expression<D>, SyntaxError<D>>,
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for FunctionWhenArm<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FunctionWhenArm {
            span: Default::default(),
            arrow_span: Default::default(),
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
            body: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
}

impl<D: Driver> FunctionWhenArm<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct FunctionWhenArmSyntax;

impl<D: Driver> Syntax<D> for FunctionWhenArmSyntax {
    type Context = WhenArmSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, span, (lhs_span, lhs), arrow_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.file.make_scope(scope);

                let lhs = parse::Expr::list_or_expr(lhs_span, lhs);
                let pattern = context
                    .ast_builder
                    .build_expr::<WhenPatternSyntax>(
                        WhenPatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
                        scope,
                    )
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs);
                let body = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(
                        ExpressionSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(FunctionWhenArm {
                    span,
                    arrow_span,
                    pattern,
                    body,
                    scope,
                }
                .into())
            },
        ))
    }
}
