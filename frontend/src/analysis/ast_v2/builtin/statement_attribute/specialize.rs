use crate::analysis::ast_v2::builtin::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct SpecializeStatementAttribute;

pub struct SpecializeStatementAttributeSyntax;

impl Syntax for SpecializeStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "specialize",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
