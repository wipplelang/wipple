use crate::{
    ast::{
        format::Format,
        pattern::{Pattern, PatternSyntax, PatternSyntaxContext},
        syntax::{Syntax, SyntaxContext, SyntaxRules},
        when_pattern::WhenPatternSyntaxContext,
        SyntaxError, WhenPattern,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct PatternWhenPattern<D: Driver> {
    pub pattern: Pattern<D>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for PatternWhenPattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(PatternWhenPattern {
            pattern: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> PatternWhenPattern<D> {
    pub fn span(&self) -> D::Span {
        self.pattern.span()
    }
}

impl<D: Driver> Format<D> for PatternWhenPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        self.pattern.format()
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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}