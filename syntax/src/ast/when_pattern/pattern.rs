use crate::{
    ast::{
        pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        when_pattern::WhenPatternSyntaxContext,
        WhenPattern,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct PatternWhenPattern<D: Driver> {
    pub pattern: Pattern<D>,
}

impl<D: Driver> PatternWhenPattern<D> {
    pub fn span(&self) -> D::Span {
        self.pattern.span()
    }
}

impl<D: Driver> From<Pattern<D>> for WhenPattern<D> {
    fn from(pattern: Pattern<D>) -> Self {
        PatternWhenPattern { pattern }.into()
    }
}

pub struct PatternWhenPatternSyntax;

impl<D: Driver> Syntax<D> for PatternWhenPatternSyntax {
    type Context = WhenPatternSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().combine(PatternSyntax::rules())
    }
}

impl<D: Driver> From<WhenPatternSyntaxContext<D>> for PatternSyntaxContext<D> {
    fn from(context: WhenPatternSyntaxContext<D>) -> Self {
        PatternSyntaxContext::new(context.ast_builder)
            .with_statement_attributes(context.statement_attributes.unwrap())
    }
}
