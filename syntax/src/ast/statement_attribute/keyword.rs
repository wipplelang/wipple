use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct KeywordStatementAttribute<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for KeywordStatementAttribute<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(KeywordStatementAttribute {
            span: Default::default(),
        })
    }
}

impl<D: Driver> KeywordStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct KeywordStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for KeywordStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "keyword",
            |context, span, _keyword_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`keyword` does not accept parameters");
                }

                let attribute = KeywordStatementAttribute { span };

                context.statement_attributes.unwrap().lock().keyword = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
