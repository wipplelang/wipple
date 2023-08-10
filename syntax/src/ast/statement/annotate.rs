use crate::{
    ast::{
        format::Format,
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
    pub value: Result<AnnotatedName<D>, Result<Expression<D>, SyntaxError<D>>>,
    pub annotation: Result<ConstantTypeAnnotation<D>, SyntaxError<D>>,
    pub attributes: StatementAttributes<D>,
}

#[derive(Debug, Clone)]
pub struct AnnotatedName<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
}

impl<D: Driver> AnnotateStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for AnnotateStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{}{} :: {}",
            self.attributes.format()?,
            match self.value {
                Ok(name) => name.name.as_ref().to_string(),
                Err(expr) => expr?.format()?,
            },
            self.annotation?.format()?,
        ))
    }
}

pub struct AnnotateStatementSyntax;

impl<D: Driver> Syntax<D> for AnnotateStatementSyntax {
    type Context = StatementSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "::",
            OperatorAssociativity::None,
            |context,
             span,
             (lhs_span, mut lhs_exprs),
             colon_span,
             (rhs_span, rhs_exprs),
             scope_set| async move {
                let value = if lhs_exprs.len() == 1 {
                    let lhs = lhs_exprs.pop().unwrap();
                    match lhs.kind {
                        parse::ExprKind::Name(name, _) => Ok(AnnotatedName {
                            span: lhs.span,
                            name,
                        }),
                        _ => {
                            let expr = context
                                .ast_builder
                                .build_expr::<ExpressionSyntax>(
                                    ExpressionSyntaxContext::new(context.ast_builder.clone())
                                        .with_statement_attributes(
                                            context.statement_attributes.as_ref().unwrap().clone(),
                                        ),
                                    lhs,
                                    scope_set.clone(),
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
                            scope_set.clone(),
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
                        scope_set,
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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![crate::ast::BuiltinSyntaxDefinition::ANNOTATE]
}
