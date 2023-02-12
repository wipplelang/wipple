use crate::{
    analysis::ast::{
        pattern::PatternSyntaxContext,
        syntax::{
            FileBodySyntaxContext, OperatorAssociativity, Syntax, SyntaxContext, SyntaxError,
            SyntaxRule, SyntaxRules,
        },
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Pattern, PatternSyntax,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct WherePattern {
    pub where_span: Span,
    pub pattern: Result<Box<Pattern>, SyntaxError>,
    pub condition: Result<Box<Expression>, SyntaxError>,
}

pub struct WherePatternSyntax;

impl Syntax for WherePatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let pattern = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs,scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let condition = context
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

                Ok(WherePattern {
                    where_span: operator_span,
                    pattern: pattern.map(Box::new),
                    condition: condition.map(Box::new),
                }
                .into())
            },
        ))
    }
}
