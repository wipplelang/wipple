use crate::{
    ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        ConstantTypeAnnotation, ConstantTypeAnnotationSyntax, ConstantTypeAnnotationSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, StatementAttributes,
        StatementSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct AnnotateStatement<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub value: Result<(D::Span, D::InternedString), Result<Expression<D>, SyntaxError<D>>>,
    pub annotation: Result<ConstantTypeAnnotation<D>, SyntaxError<D>>,
    pub attributes: StatementAttributes<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for AnnotateStatement<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(AnnotateStatement {
            span: Default::default(),
            colon_span: Default::default(),
            value: arbitrary::Arbitrary::arbitrary(u)?,
            annotation: arbitrary::Arbitrary::arbitrary(u)?,
            attributes: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> AnnotateStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct AnnotateStatementSyntax;

impl<D: Driver> Syntax<D> for AnnotateStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "::",
            OperatorAssociativity::None,
            |context, span, (lhs_span, mut lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let value = if lhs_exprs.len() == 1 {
                    let lhs = lhs_exprs.pop().unwrap();
                    match lhs.kind {
                        parse::ExprKind::Name(name, _) => Ok((lhs.span, name)),
                        _ => {
                            let expr = context
                                .ast_builder
                                .build_expr::<ExpressionSyntax>(
                                    ExpressionSyntaxContext::new(context.ast_builder.clone())
                                        .with_statement_attributes(
                                            context.statement_attributes.as_ref().unwrap().clone(),
                                        ),
                                    lhs,
                                    scope,
                                )
                                .await;

                            Err(expr)
                        }
                    }
                } else {
                    let lhs = parse::Expr::list(lhs_span, lhs_exprs);

                    let expr = context
                        .ast_builder
                        .build_expr::<ExpressionSyntax>(
                            ExpressionSyntaxContext::new(context.ast_builder.clone())
                                .with_statement_attributes(
                                    context.statement_attributes.as_ref().unwrap().clone(),
                                ),
                            lhs,
                            scope,
                        )
                        .await;

                    Err(expr)
                };

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<ConstantTypeAnnotationSyntax>(
                        ConstantTypeAnnotationSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AnnotateStatement {
                    span,
                    colon_span,
                    value,
                    annotation: ty,
                    attributes: context.statement_attributes.unwrap().lock().clone(),
                }
                .into())
            },
        ))
    }
}
