use crate::{
    analysis::ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        AssignmentPattern,
    },
    parse::Span,
};

#[derive(Debug, Clone)]
pub struct PatternAssignmentPattern {
    pub pattern: Pattern,
}

impl PatternAssignmentPattern {
    pub fn span(&self) -> Span {
        self.pattern.span()
    }
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
