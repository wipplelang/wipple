use crate::{
    analysis::ast::{
        pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        when_pattern::WhenPatternSyntaxContext,
        WhenPattern,
    },
    parse::SpanList,
};

#[derive(Debug, Clone)]
pub struct PatternWhenPattern {
    pub pattern: Pattern,
}

impl PatternWhenPattern {
    pub fn span(&self) -> SpanList {
        self.pattern.span()
    }
}

impl From<Pattern> for WhenPattern {
    fn from(pattern: Pattern) -> Self {
        PatternWhenPattern { pattern }.into()
    }
}

pub struct PatternWhenPatternSyntax;

impl Syntax for PatternWhenPatternSyntax {
    type Context = WhenPatternSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().combine(PatternSyntax::rules())
    }
}

impl From<WhenPatternSyntaxContext> for PatternSyntaxContext {
    fn from(context: WhenPatternSyntaxContext) -> Self {
        PatternSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
