use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct AnnotateExpression {
    pub span: SpanList,
    pub colon_span: SpanList,
    pub expr: Result<Box<Expression>, SyntaxError>,
    pub ty: Result<Type, SyntaxError>,
}

impl AnnotateExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct AnnotateExpressionSyntax;

impl Syntax for AnnotateExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
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
