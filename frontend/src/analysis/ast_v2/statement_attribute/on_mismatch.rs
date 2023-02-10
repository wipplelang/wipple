use crate::analysis::ast_v2::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct OnMismatchStatementAttribute;

pub struct OnMismatchStatementAttributeSyntax;

impl Syntax for OnMismatchStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "on-mismatch",
            |context, span, exprs| async move {
                todo!();
            },
        ))
    }
}
