use crate::{
    analysis::ast_v2::{
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        when_arm::WhenArmSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Pattern, PatternSyntax,
        PatternSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct FunctionWhenArm {
    pub arrow_span: Span,
    pub pattern: Result<Pattern, SyntaxError>,
    pub body: Result<Expression, SyntaxError>,
}

pub struct FunctionWhenArmSyntax;

impl Syntax for FunctionWhenArmSyntax {
    type Context = WhenArmSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "->",
            OperatorAssociativity::Right,
            |context, (lhs_span, lhs), operator_span, (rhs_span, rhs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(
                        PatternSyntaxContext::new(context.ast_builder.clone())
                            .with_statement_attributes(
                                context.statement_attributes.as_ref().unwrap().clone(),
                            ),
                        lhs,
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
                    )
                    .await;

                Ok(FunctionWhenArm {
                    arrow_span: operator_span,
                    pattern,
                    body,
                }
                .into())
            },
        ))
    }
}
