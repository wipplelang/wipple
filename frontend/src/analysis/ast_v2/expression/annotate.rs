use crate::{
    analysis::ast_v2::{
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct AnnotateExpression {
    pub colon_span: Span,
    pub expr: Result<Box<Expression>, SyntaxError>,
    pub ty: Result<Type, SyntaxError>,
}

pub struct AnnotateExpressionSyntax;

impl Syntax for AnnotateExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);
                let expr = context
                    .ast_builder
                    .build_expr::<ExpressionSyntax>(context.clone(), lhs)
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);
                let ty = context
                    .ast_builder
                    .build_expr::<TypeSyntax>(
                        TypeSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        rhs,
                    )
                    .await;

                Ok(AnnotateExpression {
                    colon_span: operator_span,
                    expr: expr.map(Box::new),
                    ty,
                }
                .into())
            },
        ))
    }
}
