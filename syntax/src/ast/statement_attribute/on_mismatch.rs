use crate::{
    ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    parse, Driver,
};

#[derive(Debug, Clone)]
pub struct OnMismatchStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub on_mismatch_span: D::Span,
    pub type_parameter: Option<(D::Span, D::InternedString)>,
    pub message_span: D::Span,
    pub message: D::InternedString,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for OnMismatchStatementAttribute<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(OnMismatchStatementAttribute {
            span: Default::default(),
            on_mismatch_span: Default::default(),
            type_parameter: Default::default(),
            message_span: Default::default(),
            message: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> OnMismatchStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

pub struct OnMismatchStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for OnMismatchStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "on-mismatch",
            |context, span, on_mismatch_span, mut exprs, _scope| async move {
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
                        let type_parameter = match type_parameter_expr.kind {
                            parse::ExprKind::Name(name, _) => name,
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
                            type_parameter: Some((type_parameter_expr.span, type_parameter)),
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
