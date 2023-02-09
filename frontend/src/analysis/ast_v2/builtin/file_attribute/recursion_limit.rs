use crate::analysis::ast_v2::builtin::{
    file_attribute::FileAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct RecursionLimitFileAttribute;

pub struct RecursionLimitFileAttributeSyntax;

impl Syntax for RecursionLimitFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "recursion-limit",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
