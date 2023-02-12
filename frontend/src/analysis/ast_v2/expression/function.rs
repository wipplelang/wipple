use crate::{
    analysis::ast_v2::{
        expression::ExpressionSyntaxContext,
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub arrow_span: Span,
    pub pattern: Result<Pattern, SyntaxError>,
    pub body: Result<Box<Expression>, SyntaxError>,
}

pub struct FunctionExpressionSyntax;

impl Syntax for FunctionExpressionSyntax {
    type Context = ExpressionSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs), scope| async move {
                let scope = context.ast_builder.child_scope(scope);

                let lhs = parse::Expr::list(lhs_span, lhs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(
                        PatternSyntaxContext::new(context.ast_builder.clone())
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
                    .build_expr::<ExpressionSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(FunctionExpression {
                    arrow_span: operator_span,
                    pattern,
                    body: body.map(Box::new),
                }
                .into())
            },
        ))
    }
}
