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
pub struct HelpTemplateStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub help_template_span: D::Span,
    pub help_template_text_span: D::Span,
    pub help_template_text: D::InternedString,
}

impl<D: Driver> HelpTemplateStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for HelpTemplateStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

#[derive(Debug, Clone)]
pub struct HelpTemplate<S = String> {
    pub items: Vec<HelpTemplateItem<S>>,
}

#[derive(Debug, Clone)]
pub enum HelpTemplateItem<S = String> {
    Code(S),
    Placeholder(S),
}

impl<S> Default for HelpTemplate<S> {
    fn default() -> Self {
        Self {
            items: Default::default(),
        }
    }
}

pub struct HelpTemplateStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for HelpTemplateStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "help-template",
            |context, span, help_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`help-template` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let help_template_text = match expr.kind {
                    parse::ExprKind::Text(text) => text.ignoring_escaped_underscores(),
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = HelpTemplateStatementAttribute {
                    span,
                    help_template_span: help_span,
                    help_template_text_span: expr.span,
                    help_template_text,
                };

                context.statement_attributes.unwrap().lock().help_template =
                    Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
