use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct SpecializeStatementAttribute<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for SpecializeStatementAttribute<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(SpecializeStatementAttribute {
            span: Default::default(),
        })
    }
}

impl<D: Driver> SpecializeStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct SpecializeStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for SpecializeStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "specialize",
            |context, span, _specialize_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`specialize` does not accept parameters");
                }

                let attribute = SpecializeStatementAttribute { span };

                context.statement_attributes.unwrap().lock().specialize = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
