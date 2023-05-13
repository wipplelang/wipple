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
pub struct LanguageItemStatementAttribute<D: Driver> {
    pub span: D::Span,
    pub language_span: D::Span,
    pub language_item_span: D::Span,
    pub language_item_kind: LanguageItemStatementAttributeKind,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for LanguageItemStatementAttribute<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(LanguageItemStatementAttribute {
            span: Default::default(),
            language_span: Default::default(),
            language_item_span: Default::default(),
            language_item_kind: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> LanguageItemStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for LanguageItemStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
    }
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum LanguageItemStatementAttributeKind {
    Boolean,
    Show,
}

pub struct LanguageItemStatementAttributeSyntax;

impl<D: Driver> Syntax<D> for LanguageItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new().with(SyntaxRule::<D, Self>::function(
            "language",
            |context, span, language_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context
                        .ast_builder
                        .driver
                        .syntax_error(span, "`language` accepts 1 input");

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let language_item = match expr.kind {
                    parse::ExprKind::Text(text, _) => {
                        match text.as_ref().parse::<LanguageItemStatementAttributeKind>() {
                            Ok(item) => item,
                            Err(_) => {
                                context.ast_builder.driver.syntax_error_with([
                                    (expr.span, String::from("invalid `language` item")),
                                    (
                                        expr.span,
                                        String::from("see the Wipple source code for a list of language items"),
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

                let attribute = LanguageItemStatementAttribute {
                    span,
                    language_span,
                    language_item_span: expr.span,
                    language_item_kind: language_item,
                };

                context.statement_attributes.unwrap().lock().language_item =
                    Some(attribute.clone());

                Ok(attribute.into())
            },
        ))
    }
}
