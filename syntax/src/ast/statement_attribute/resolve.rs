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
pub struct ResolveStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub resolve_span: D::Span,
    pub message: Result<D::InternedString, SyntaxError<D>>,
    pub replacement: parse::Expr<D>,
}

impl<D: Driver> ResolveStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ResolveStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct ResolveStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for ResolveStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "resolve",
            |context, span, resolve_span, exprs, _scope_set| async move {
                if exprs.len() != 2 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`resolve` accepts 2 inputs");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let mut exprs = exprs.into_iter();

                let message = exprs.next().unwrap();
                let message = match message.kind {
                    parse::ExprKind::Text(text) => Ok(text.ignoring_escaped_underscores()),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(message.span, "expected text");

                        Err(context.ast_builder.syntax_error(span))
                    }
                };

                let replacement = exprs.next().unwrap();

                let attribute = ResolveStatementAttribute {
                    span,
                    resolve_span,
                    message,
                    replacement,
                };

                context.statement_attributes.unwrap().lock().resolve = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
