use crate::analysis::ast_v2::builtin::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct HelpStatementAttribute;

pub struct HelpStatementAttributeSyntax;

impl Syntax for HelpStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "help",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
