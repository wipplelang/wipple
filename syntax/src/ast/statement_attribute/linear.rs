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
pub struct LinearStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> LinearStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for LinearStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct LinearStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for LinearStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "linear",
            |context, span, _linear_span, exprs, _scope| async move {
                if !exprs.is_empty() {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`linear` does not accept parameters");
                }

                let attribute = LinearStatementAttribute { span };

                context.statement_attributes.unwrap().lock().linear = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
