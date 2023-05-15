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
pub struct DiagnosticItemStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub diagnostic_span: D::Span,
    pub diagnostic_item_span: D::Span,
    pub diagnostic_item_kind: DiagnosticItemStatementAttributeKind,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for DiagnosticItemStatementAttribute<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(DiagnosticItemStatementAttribute {
            span: Default::default(),
            diagnostic_span: Default::default(),
            diagnostic_item_span: Default::default(),
            diagnostic_item_kind: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> DiagnosticItemStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum DiagnosticItemStatementAttributeKind {
    AcceptsText,
}

impl<D: Driver> Format<D> for DiagnosticItemStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

pub struct DiagnosticItemStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for DiagnosticItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "diagnostic",
            |context, span, diagnostic_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`diagnostic` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let diagnostic_item = match expr.kind {
                    parse::ExprKind::Text(text, _) => {
                        match text.as_ref().parse::<DiagnosticItemStatementAttributeKind>() {
                            Ok(item) => item,
                            Err(_) => {
                                context.ast_builder.driver.syntax_error_with([
                                    (expr.span, String::from("invalid `diagnostic` item")),
                                    (
                                        expr.span,
                                        String::from("see the Wipple source code for a list of diagnostic items"),
                                    ),
                                ]);

                                return Err(context.ast_builder.syntax_error(span));
                            }
                        }
                    }
                    _ => {
                        context
                            .ast_builder
                            .driver
                            .syntax_error(expr.span, "expected text");

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
