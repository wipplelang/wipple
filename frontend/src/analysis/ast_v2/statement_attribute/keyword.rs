use crate::analysis::ast_v2::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct KeywordStatementAttribute;

pub struct KeywordStatementAttributeSyntax;

impl Syntax for KeywordStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "keyword",
            |context, span, exprs| async move {
                todo!();
            },
        ))
    }
}
