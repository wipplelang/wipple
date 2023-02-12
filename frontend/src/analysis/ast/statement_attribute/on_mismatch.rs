use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct OnMismatchStatementAttribute {
    pub on_mismatch_span: Span,
    pub type_parameter: Option<(Span, InternedString)>,
    pub message_span: Span,
    pub message: InternedString,
}

pub struct OnMismatchStatementAttributeSyntax;

impl Syntax for OnMismatchStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "on-mismatch",
            |context, span, mut exprs, _scope| async move {
                let attribute = match exprs.len() {
                    1 => {
                        let expr = exprs.pop().unwrap();
                        let message = match expr.kind {
                            parse::ExprKind::Text(text) => text,
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "expected text here")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        OnMismatchStatementAttribute {
                            on_mismatch_span: span,
                            type_parameter: None,
                            message_span: expr.span,
                            message,
                        }
                    }
                    2 => {
                        let mut exprs = exprs.into_iter();

                        let type_parameter_expr = exprs.next().unwrap();
                        let type_parameter = match type_parameter_expr.kind {
                            parse::ExprKind::Name(name) => name,
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "expected a type parameter here")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        let message_expr = exprs.next().unwrap();
                        let message = match message_expr.kind {
                            parse::ExprKind::Text(text) => text,
                            _ => {
                                context.ast_builder.compiler.add_error(
                                    "syntax error",
                                    vec![Note::primary(span, "expected text here")],
                                );

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        OnMismatchStatementAttribute {
                            on_mismatch_span: span,
                            type_parameter: Some((type_parameter_expr.span, type_parameter)),
                            message_span: message_expr.span,
                            message,
                        }
                    }
                    _ => {
                        context.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "`on-mismatch` accepts 1-2 inputs")],
                        );

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                context.statement_attributes.unwrap().lock().on_mismatch = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
