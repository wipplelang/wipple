use crate::{
    analysis::ast::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::{self, SpanList},
};

#[derive(Debug, Clone)]
pub struct LanguageItemStatementAttribute {
    pub span: SpanList,
    pub language_span: SpanList,
    pub language_item_span: SpanList,
    pub language_item_kind: LanguageItemStatementAttributeKind,
}

impl LanguageItemStatementAttribute {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItemStatementAttributeKind {
    Boolean,
    Show,
}

pub struct LanguageItemStatementAttributeSyntax;

impl Syntax for LanguageItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "language",
            |context, span, language_span, mut exprs, _scope| async move {
                if exprs.len() != 1 {
                    context.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(span, "`language` accepts 1 input")],
                    );

                    return Err(context.ast_builder.syntax_error(span));
                }

                let expr = exprs.pop().unwrap();
                let language_item = match expr.kind {
                    parse::ExprKind::Text(text) => {
                        match text.parse::<LanguageItemStatementAttributeKind>() {
                            Ok(item) => item,
                            Err(_) => {
                                context.ast_builder.compiler.add_error(
                                    "invalid `language` item",
                                    vec![Note::primary(
                                        expr.span,
                                        "see the Wipple source code for a list of language items",
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
