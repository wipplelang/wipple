mod instance;
mod type_function;

use crate::analysis::ast_v2::builtin::SyntaxContext;
use instance::*;
use type_function::*;

impl<'a> TryFrom<SyntaxContext<'a>> for InstanceAssignmentPatternSyntaxContext<'a> {
    type Error = ();

    fn try_from(context: SyntaxContext) -> Result<Self, Self::Error> {
        todo!()
    }
}

syntax_context_group! {
    pub enum AssignmentPatternSyntaxContext {
        InstanceAssignmentPattern,
        TypeFunctionAssignmentPattern,
    }
}

syntax_group! {
    pub enum AssignmentPattern<AssignmentPatternSyntaxContext> {
        Instance,
        TypeFunction,
    }
}
