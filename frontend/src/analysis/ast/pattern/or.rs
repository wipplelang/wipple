use crate::{
    analysis::ast::{
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

impl OrPattern {
    pub fn span(&self) -> Span {
        let left_span = match &self.left {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        let right_span = match &self.right {
            Ok(pattern) => pattern.span(),
            Err(error) => error.span,
        };

        Span::join(left_span, right_span)
    }
}

pub struct OrPatternSyntax;

impl Syntax for OrPatternSyntax {
    type Context = PatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::operator(
            "or",
            OperatorAssociativity::Left,
            |context, (lhs_span, lhs_exprs), operator_span, (rhs_span, rhs_exprs), scope| async move {
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
                    or_span: operator_span,
                    left: left.map(Box::new),
                    right: right.map(Box::new),
                }
                .into())
            },
        ))
    }
}