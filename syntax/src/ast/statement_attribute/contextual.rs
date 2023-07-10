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
pub struct ContextualStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> ContextualStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ContextualStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
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

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
