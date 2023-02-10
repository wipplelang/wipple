use crate::analysis::ast_v2::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct OnUnimplementedStatementAttribute;

pub struct OnUnimplementedStatementAttributeSyntax;

impl Syntax for OnUnimplementedStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "on-unimplemented",
            |context, span, exprs| async move {
                todo!();
            },
        ))
    }
}
