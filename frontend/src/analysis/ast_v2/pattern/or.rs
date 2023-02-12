use crate::{
    analysis::ast_v2::{
        syntax::{OperatorAssociativity, Syntax, SyntaxError, SyntaxRule, SyntaxRules},
        Pattern, PatternSyntax, PatternSyntaxContext,
    },
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct OrPattern {
    pub or_span: Span,
    pub left: Result<Box<Pattern>, SyntaxError>,
    pub right: Result<Box<Pattern>, SyntaxError>,
}

pub struct OrPatternSyntax;

impl Syntax for OrPatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "or",
            OperatorAssociativity::Left,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs)| async move {
                let lhs = parse::Expr::list(lhs_span, lhs_exprs);
                let left = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), lhs)
                    .await;

                let rhs = parse::Expr::list(rhs_span, rhs_exprs);
                let right = context
                    .ast_builder
                    .build_expr::<PatternSyntax>(context.clone(), rhs)
                    .await;

                Ok(OrPattern {
                    or_span: operator_span,
                    left: left.map(Box::new),
                    right: right.map(Box::new),
                }
                .into())
            },
        ))
    }
}
