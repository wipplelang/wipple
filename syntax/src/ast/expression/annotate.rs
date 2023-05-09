use crate::{
    ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct AnnotateExpression<D: Driver> {
    pub span: D::Span,
    pub colon_span: D::Span,
    pub expr: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub ty: Result<Type<D>, SyntaxError<D>>,
}

impl<D: Driver> AnnotateExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct AnnotateExpressionSyntax;

impl<D: Driver> Syntax<D> for AnnotateExpressionSyntax {
    type Context = ExpressionSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, lhs_exprs), colon_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);
                let expr = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                        scope,
                    )
                    .await;

                Ok(AnnotateExpression {
                    span,
                    colon_span,
                    expr: expr.map(Box::new),
                    ty,
                }
                .into())
            },
        ))
    }
}
