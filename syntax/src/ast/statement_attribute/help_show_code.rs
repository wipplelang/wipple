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
pub struct HelpShowCodeStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> HelpShowCodeStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpShowCodeStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct HelpShowCodeStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpShowCodeStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-show-code",
            |context, span, _allow_overlapping_instances_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context.ast_builder.driver.syntax_error(
                        span,
                        "`allow-overlapping-instances` does not accept parameters",
                    );
                }

                let attribute = HelpShowCodeStatementAttribute { span };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .help_show_code = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
