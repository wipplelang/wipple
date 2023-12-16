use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct HelpAlternativeStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub diagnostic_span: D::Span,
    pub help_alternative: parse::Expr<D>,
}

impl<D: Driver> HelpAlternativeStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpAlternativeStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct HelpAlternativeStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpAlternativeStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-alternative",
            |context, span, diagnostic_span, mut exprs, _scope_set| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`diagnostic` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let help_alternative = exprs.pop().unwrap();

                let attribute = HelpAlternativeStatementAttribute {
                    span,
                    diagnostic_span,
                    help_alternative,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .help_alternatives
                    .push(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
