use crate::analysis::ast_v2::builtin::{
    file_attribute::FileAttributeSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct NoStdFileAttribute;

pub struct NoStdFileAttributeSyntax;

impl Syntax for NoStdFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "no-std",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
