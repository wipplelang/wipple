use crate::{
    ast::{
        file_attribute::FileAttributeSyntaxContext,
        format::Format,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct HelpUrlFileAttribute<D: Driver> {
    pub span: D::Span,
    pub help_url_span: D::Span,
    pub help_url_text_span: D::Span,
    pub help_url_text: D::InternedString,
}

impl<D: Driver> HelpUrlFileAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpUrlFileAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("[[help-url {:?}]]", self.help_url_text))
    }
}

pub struct HelpUrlFileAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpUrlFileAttributeSyntax {
    type Context = FileAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-url",
            |context, span, help_url_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`help-url` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let help_url_text = match expr.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = HelpUrlFileAttribute {
                    span,
                    help_url_span,
                    help_url_text_span: expr.span,
                    help_url_text,
                };

                context.ast_builder.attributes.lock().help_url = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
