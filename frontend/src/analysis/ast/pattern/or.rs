use crate::{
    analysis::ast::{
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct OrPattern {
    pub span: SpanList,
    pub or_span: SpanList,
    pub left: Result<Box<Pattern>, SyntaxError>,
    pub right: Result<Box<Pattern>, SyntaxError>,
}

impl OrPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct OrPatternSyntax;

impl Syntax for OrPatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "or",
            OperatorAssociativity::Left,
            |context, span, (lhs_span, lhs_exprs), or_span, (rhs_span, rhs_exprs), scope| async move {
                let lhs = parse::Expr::list_or_expr(lhs_span, lhs_exprs);
                let left = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs, scope)
                    .await;

                let rhs = parse::Expr::list_or_expr(rhs_span, rhs_exprs);
                let right = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), rhs, scope)
                    .await;

                Ok(OrPattern {
                    span,
                    or_span,
                    left: left.map(Box::new),
                    right: right.map(Box::new),
                }
                .into())
            },
        ))
    }
}
