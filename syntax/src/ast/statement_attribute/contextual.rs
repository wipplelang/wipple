use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct ContextualStatementAttribute<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for ContextualStatementAttribute<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ContextualStatementAttribute {
            span: Default::default(),
        })
    }
}
impl<D: Driver> ContextualStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct ContextualStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for ContextualStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "context",
            |context, span, _specialize_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`contextual` does not accept parameters");
                }

                let attribute = ContextualStatementAttribute { span };

                context.statement_attributes.unwrap().lock().contextual = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
