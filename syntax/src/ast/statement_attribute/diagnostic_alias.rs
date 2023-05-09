use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct DiagnosticAliasStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub diagnostic_span: D::Span,
    pub diagnostic_alias_span: D::Span,
    pub diagnostic_alias: D::InternedString,
}

impl<D: Driver> DiagnosticAliasStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct DiagnosticAliasStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for DiagnosticAliasStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "diagnostic-alias",
            |context, span, diagnostic_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`diagnostic` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let diagnostic_alias = match expr.kind {
                    parse::ExprKind::Text(text, _) => text,
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = DiagnosticAliasStatementAttribute {
                    span,
                    diagnostic_span,
                    diagnostic_alias_span: expr.span,
                    diagnostic_alias,
                };

                context
                    .statement_attributes
                    .unwrap()
                    .lock()
                    .diagnostic_aliases
                    .push(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
