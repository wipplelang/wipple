use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind, LanguageItem,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::{collections::HashMap, str::FromStr};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct LanguageSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for LanguageSyntax {
    fn name(self) -> &'static str {
        "language"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("item")),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("expr")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let item = vars.remove(&InternedString::new("item")).unwrap();
        let expr = vars.remove(&InternedString::new("expr")).unwrap();

        let statement_attributes = match context {
            Some(Context::StatementAttributes(attributes)) => attributes,
            _ => {
                expander.compiler.add_error(
                    "`language` may only be used as an attribute",
                    vec![Note::primary(
                        span,
                        "try putting this between `[` and `]` brackets",
                    )],
                );

                return expr;
            }
        };

        let item = match item.kind {
            ExpressionKind::Text(text) => text,
            _ => {
                expander.compiler.add_error(
                    "`language` requires a `Text` value",
                    vec![Note::primary(span, "try removing this")],
                );

                return expr;
            }
        };

        let item = match LanguageItem::from_str(&item) {
            Ok(item) => item,
            Err(_) => {
                expander.compiler.add_error(
                    "invalid language item",
                    vec![Note::primary(
                        span,
                        "see the Wipple source code for a list of language items",
                    )],
                );

                return expr;
            }
        };

        statement_attributes.language_item = Some(item);

        expr
    }
}
