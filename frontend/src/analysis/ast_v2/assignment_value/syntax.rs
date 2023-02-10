use crate::analysis::ast_v2::{
    assignment_value::AssignmentValueSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct SyntaxAssignmentValue;

pub struct SyntaxAssignmentValueSyntax;

impl Syntax for SyntaxAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "syntax",
            |context, span, exprs| async move {
                todo!();
            },
        ))
    }
}
