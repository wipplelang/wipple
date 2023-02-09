use crate::analysis::ast_v2::builtin::{
    assignment_value::AssignmentValueSyntaxContext,
    syntax::{Syntax, SyntaxRule, SyntaxRules},
};

#[derive(Debug, Clone)]
pub struct TraitAssignmentValue;

pub struct TraitAssignmentValueSyntax;

impl Syntax for TraitAssignmentValueSyntax {
    type Context = AssignmentValueSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "trait",
            |context, exprs| async move {
                todo!();
            },
        ))
    }
}
