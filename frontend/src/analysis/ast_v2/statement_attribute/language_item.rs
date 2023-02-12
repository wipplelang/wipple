use crate::{
    analysis::ast_v2::{
        statement_attribute::StatementAttributeSyntaxContext,
        syntax::{Syntax, SyntaxRule, SyntaxRules},
    },
    diagnostics::Note,
    parse::{self, Span},
};

#[derive(Debug, Clone)]
pub struct LanguageItemStatementAttribute {
    pub language_span: Span,
    pub language_item_span: Span,
    pub language_item_kind: LanguageItemStatementAttributeKind,
}

#[derive(Debug, Clone, Copy, strum::EnumString)]
#[strum(serialize_all = "kebab-case")]
pub enum LanguageItemStatementAttributeKind {
    Boolean,
}

pub struct LanguageItemStatementAttributeSyntax;

impl Syntax for LanguageItemStatementAttributeSyntax {
    type Context = StatementAttributeSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new().with(SyntaxRule::<Self>::function(
            "language",
            |context, span, mut exprs, _scope| async move {
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
                    language_span: span,
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
