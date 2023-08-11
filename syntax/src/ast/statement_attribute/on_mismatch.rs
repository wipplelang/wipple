use crate::{
    ast::{
        format::Format,
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
        SyntaxError,
    },
    parse, Driver,
};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct OnMismatchStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub on_mismatch_span: D::Span,
    pub type_parameter: Option<(D::Span, D::InternedString, HashSet<D::Scope>)>,
    pub message_span: D::Span,
    pub message: D::InternedString,
}

impl<D: Driver> OnMismatchStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for OnMismatchStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct OnMismatchStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OnMismatchStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "on-mismatch",
            |context, span, on_mismatch_span, mut exprs, scope_set| async move {
                let attribute = match exprs.len() {
                    1 => {
                        let expr = exprs.pop().unwrap();
                        let message = match expr.kind {
                            parse::ExprKind::Text(text, _) => text,
                            _ => {
                                context
                                    .ast_builder
                                    .driver
                                    .syntax_error(span, "expected text here");

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        OnMismatchStatementAttribute {
                            span,
                            on_mismatch_span,
                            type_parameter: None,
                            message_span: expr.span,
                            message,
                        }
                    }
                    2 => {
                        let mut exprs = exprs.into_iter();

                        let type_parameter_expr = exprs.next().unwrap();
                        let (type_parameter, type_parameter_scope) = match type_parameter_expr.kind
                        {
                            parse::ExprKind::Name(name, scope) => {
                                (name, scope.unwrap_or_else(|| scope_set.lock().clone()))
                            }
                            _ => {
                                context
                                    .ast_builder
                                    .driver
                                    .syntax_error(span, "expected a type parameter here");

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        let message_expr = exprs.next().unwrap();
                        let message = match message_expr.kind {
                            parse::ExprKind::Text(text, _) => text,
                            _ => {
                                context
                                    .ast_builder
                                    .driver
                                    .syntax_error(span, "expected text here");

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        };

                        OnMismatchStatementAttribute {
                            span,
                            on_mismatch_span,
                            type_parameter: Some((
                                type_parameter_expr.span,
                                type_parameter,
                                type_parameter_scope,
                            )),
                            message_span: message_expr.span,
                            message,
                        }
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(span, "`on-mismatch` accepts 1-2 inputs");

                        return Err(context.ast_builder.syntax_error(span));
                    }
                };

                context.statement_attributes.unwrap().lock().on_mismatch = Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}

pub(crate) fn builtin_syntax_definitions() -> Vec<crate::ast::BuiltinSyntaxDefinition> {
    vec![]
}
