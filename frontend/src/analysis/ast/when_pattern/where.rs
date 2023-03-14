use crate::{
    analysis::ast::{
        syntax::{
            OperatorAssociativity, Syntax, SyntaxContext, SyntaxError, SyntaxRule, SyntaxRules,
        },
        when_pattern::WhenPatternSyntaxContext,
        Expression, ExpressionSyntax, ExpressionSyntaxContext, Pattern, PatternSyntax,
        PatternSyntaxContext,
    },
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct WhereWhenPattern {
    pub span: SpanList,
    pub where_span: SpanList,
    pub pattern: Result<Box<Pattern>, SyntaxError>,
    pub condition: Result<Box<Expression>, SyntaxError>,
}

impl WhereWhenPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct WhereWhenPatternSyntax;

impl Syntax for WhereWhenPatternSyntax {
    type Context = WhenPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "where",
            OperatorAssociativity::None,
            |context, span, (lhs_span, lhs_exprs), where_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
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

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);
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

                Ok(WhereWhenPattern {
                    span,
                    where_span,
                    pattern: pattern.map(Box::new),
                    condition: condition.map(Box::new),
                }
                .into())
            },
        ))
    }
}
