use crate::analysis::ast_v2::builtin::{
    assignment_value::AssignmentValueSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct TypeFunctionAssignmentValue;

pub struct TypeFunctionAssignmentValueSyntax;

impl Syntax for TypeFunctionAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "type-function",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
