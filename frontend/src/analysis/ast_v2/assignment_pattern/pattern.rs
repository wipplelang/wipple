use crate::analysis::ast_v2::{
    assignment_pattern::AssignmentPatternSyntaxContext,
    pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
    syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxRules},
    AssignmentPattern,
};

#[derive(Debug, Clone)]
pub struct PatternAssignmentPattern {
    pub pattern: Pattern,
}

impl From<Pattern> for AssignmentPattern {
    fn from(pattern: Pattern) -> Self {
        PatternAssignmentPattern { pattern }.into()
    }
}

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
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
