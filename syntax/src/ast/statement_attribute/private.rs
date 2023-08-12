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
pub struct PrivateStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> PrivateStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for PrivateStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct PrivateStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for PrivateStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "private",
            |context, span, _private_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`private` does not accept parameters");
                }

                let attribute = PrivateStatementAttribute { span };

                context.statement_attributes.unwrap().lock().private = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
