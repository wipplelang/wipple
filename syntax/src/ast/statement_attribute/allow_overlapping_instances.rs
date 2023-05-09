use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    Driver,
};

#[derive(Debug, Clone)]
pub struct AllowOverlappingInstancesStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> AllowOverlappingInstancesStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
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
