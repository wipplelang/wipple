use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct AllowOverlappingInstancesStatementAttribute<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a>
    for AllowOverlappingInstancesStatementAttribute<D>
{
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(AllowOverlappingInstancesStatementAttribute {
            span: Default::default(),
        })
    }
}

impl<D: Driver> AllowOverlappingInstancesStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for AllowOverlappingInstancesStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct AllowOverlappingInstancesStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for AllowOverlappingInstancesStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "allow-overlapping-instances",
            |context, span, _allow_overlapping_instances_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.driver.syntax_error(
                        span,
                        "`allow-overlapping-instances` does not accept parameters",
                    );
                }

                let attribute = AllowOverlappingInstancesStatementAttribute { span };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .allow_overlapping_instances = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
