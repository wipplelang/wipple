use crate::analysis::ast_v2::builtin::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct AllowOverlappingInstancesStatementAttribute;

pub struct AllowOverlappingInstancesStatementAttributeSyntax;

impl Syntax for AllowOverlappingInstancesStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "allow-overlapping-instances",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
