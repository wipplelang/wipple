use crate::analysis::ast_v2::builtin::{Syntax, SyntaxRules};

pub struct TypeFunctionAssignmentPatternSyntax;

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentPattern {
    // TODO
}

impl<'a> Syntax<'a> for TypeFunctionAssignmentPatternSyntax {
    type Context = ();
    type Body = TypeFunctionAssignmentPattern;

    fn rules() -> SyntaxRules<'a, Self> {
        todo!()
    }
}
