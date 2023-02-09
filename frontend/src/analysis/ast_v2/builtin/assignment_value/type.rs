use crate::analysis::ast_v2::builtin::{
    assignment_value::AssignmentValueSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct TypeAssignmentValue;

pub struct TypeAssignmentValueSyntax;

impl Syntax for TypeAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "type",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
