use crate::analysis::ast_v2::builtin::{
    statement_attribute::StatementAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItemStatementAttribute {
    Boolean,
}

pub struct LanguageItemStatementAttributeSyntax;

impl Syntax for LanguageItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "language",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
