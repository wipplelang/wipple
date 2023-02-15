use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
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

impl AnnotateExpression {
    pub fn span(&self) -> Span {
        let expr_span = match &self.expr {
            Ok(expr) => expr.span(),
            Err(error) => error.span,
        };

        let ty_span = match &self.ty {
            Ok(ty) => ty.span(),
            Err(error) => error.span,
        };

        Span::join(expr_span, ty_span)
    }
}

pub struct AnnotateExpressionSyntax;

impl Syntax for AnnotateExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "::",
            OperatorAssociativity::Left,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
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
                    colon_span: operator_span,
                    expr: expr.map(Box::new),
                    ty,
                }
                .into())
            },
        ))
    }
}
