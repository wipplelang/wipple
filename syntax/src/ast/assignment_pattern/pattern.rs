use crate::{
    ast::{
        assignment_pattern::AssignmentPatternSyntaxContext,
        pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        AssignmentPattern,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct PatternAssignmentPattern<D: Driver> {
    pub pattern: Pattern<D>,
}

impl<D: Driver> PatternAssignmentPattern<D> {
    pub fn span(&self) -> D::Span {
        self.pattern.span()
    }
}

impl<D: Driver> From<Pattern<D>> for AssignmentPattern<D> {
    fn from(pattern: Pattern<D>) -> Self {
        PatternAssignmentPattern { pattern }.into()
    }
}

pub struct PatternAssignmentPatternSyntax;

impl<D: Driver> Syntax<D> for PatternAssignmentPatternSyntax {
    type Context = AssignmentPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().combine(PatternSyntax::rules())
    }
}

impl<D: Driver> From<AssignmentPatternSyntaxContext<D>> for PatternSyntaxContext<D> {
    fn from(context: AssignmentPatternSyntaxContext<D>) -> Self {
        PatternSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
