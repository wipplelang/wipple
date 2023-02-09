use crate::analysis::ast_v2::builtin::{
    assignment_pattern::AssignmentPatternSyntaxContext,
    pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
    syntax::{Syntax, SyntaxContext, SyntaxRules},
};

pub type PatternAssignmentPattern = Pattern;

pub struct PatternAssignmentPatternSyntax;

impl Syntax for PatternAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().combine(PatternSyntax::rules())
    }
}

impl From<AssignmentPatternSyntaxContext> for PatternSyntaxContext {
    fn from(context: AssignmentPatternSyntaxContext) -> Self {
        PatternSyntaxContext::new(context.ast_builder)
    }
}
