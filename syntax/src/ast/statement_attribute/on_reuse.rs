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
pub struct OnReuseStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub on_reuse_span: D::Span,
    pub on_reuse_text_span: D::Span,
    pub on_reuse_text: D::InternedString,
}

impl<D: Driver> OnReuseStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for OnReuseStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct OnReuseStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OnReuseStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "on-reuse",
            |context, span, on_reuse_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`on-reuse` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let on_reuse_text = match expr.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = OnReuseStatementAttribute {
                    span,
                    on_reuse_span,
                    on_reuse_text_span: expr.span,
                    on_reuse_text,
                };

                context.statement_attributes.unwrap().lock().on_reuse = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
