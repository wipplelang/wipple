use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct DiagnosticItemStatementAttribute {
    pub span: SpanList,
    pub diagnostic_span: SpanList,
    pub diagnostic_item_span: SpanList,
    pub diagnostic_item_kind: DiagnosticItemStatementAttributeKind,
}

impl DiagnosticItemStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum DiagnosticItemStatementAttributeKind {
    AcceptsText,
}

pub struct DiagnosticItemStatementAttributeSyntax;

impl Syntax for DiagnosticItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "diagnostic",
            |context, span, diagnostic_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`diagnostic` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let diagnostic_item = match expr.kind {
                    parse::ExprKind::Text(text) => {
                        match text.parse::<DiagnosticItemStatementAttributeKind>() {
                            Ok(item) => item,
                            Err(_) => {
                                context.ast_builder.compiler.add_error(
                                    "invalid `diagnostic` item",
                                    vec![Note::primary(
                                        expr.span,
                                        "see the Wipple source code for a list of diagnostic items",
                                    )],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        }
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected text")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                let attribute = DiagnosticItemStatementAttribute {
                    span,
                    diagnostic_span,
                    diagnostic_item_span: expr.span,
                    diagnostic_item_kind: diagnostic_item,
                };

                context.statement_attributes.unwrap().lock().diagnostic_item =
                    Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
