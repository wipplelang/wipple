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
pub struct NoReuseStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub no_reuse_span: D::Span,
    pub no_reuse_text: Option<(D::Span, D::InternedString)>,
}

impl<D: Driver> NoReuseStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NoReuseStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct NoReuseStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for NoReuseStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "no-reuse",
            |context, span, no_reuse_span, mut exprs, _scope| async move {
                if exprs.len() > 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`no-reuse` accepts at most 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let no_reuse_text = match exprs.pop() {
                    Some(expr) => match expr.kind {
                        parse::ExprKind::Text(text) => {
                            Some((expr.span, text.ignoring_escaped_underscores()))
                        }
                        _ => {
                            context
                                .ast_builder
                                .driver
                                .syntax_error(expr.span, "expected text");

                            return Err(context.ast_builder.syntax_error(span));
                        }
                    },
                    None => None,
                };

                let attribute = NoReuseStatementAttribute {
                    span,
                    no_reuse_span,
                    no_reuse_text,
                };

                context.statement_attributes.unwrap().lock().no_reuse = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
