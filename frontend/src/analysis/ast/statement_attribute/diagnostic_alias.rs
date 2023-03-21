use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct DiagnosticAliasStatementAttribute {
    pub span: SpanList,
    pub diagnostic_span: SpanList,
    pub diagnostic_alias_span: SpanList,
    pub diagnostic_alias: InternedString,
}

impl DiagnosticAliasStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

pub struct DiagnosticAliasStatementAttributeSyntax;

impl Syntax for DiagnosticAliasStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "diagnostic-alias",
            |context, span, diagnostic_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`diagnostic` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let diagnostic_alias = match expr.kind {
                    parse::ExprKind::Text(text) => text,
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

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
