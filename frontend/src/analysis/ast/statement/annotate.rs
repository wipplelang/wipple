use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        ConstantTypeAnnotation, ConstantTypeAnnotationSyntax, ConstantTypeAnnotationSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, StatementAttributes,
        StatementSyntaxContext,
    },
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct AnnotateStatement {
    pub span: SpanList,
    pub colon_span: SpanList,
    pub value: Result<(SpanList, InternedString), Result<Expression, SyntaxError>>,
    pub annotation: Result<ConstantTypeAnnotation, SyntaxError>,
    pub attributes: StatementAttributes,
}

impl AnnotateStatement {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct AnnotateStatementSyntax;

impl Syntax for AnnotateStatementSyntax {
    type Context = StatementSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
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
