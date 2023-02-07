use crate::{
    analysis::expand::{
        syntax::BuiltinSyntaxVisitor, Context, Expander, Expression, ExpressionKind,
    },
    diagnostics::Note,
    helpers::InternedString,
    parse::Span,
    ScopeId,
};
use async_trait::async_trait;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct FormatSyntax;

#[async_trait]
impl BuiltinSyntaxVisitor for FormatSyntax {
    fn name(self) -> &'static str {
        "format"
    }

    fn pattern(self) -> Vec<Expression> {
        vec![
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Name(InternedString::new(self.name())),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::Variable(InternedString::new("text")),
            },
            Expression {
                span: Span::builtin(),
                kind: ExpressionKind::RepeatedVariable(InternedString::new("inputs")),
            },
        ]
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _context: Option<Context<'_>>,
        _scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression {
        let format_text = vars.remove(&InternedString::new("text")).unwrap();

        let inputs = match vars.remove(&InternedString::new("inputs")).unwrap().kind {
            ExpressionKind::List(exprs) => exprs,
            _ => unreachable!(),
        };

        if let ExpressionKind::Text(text) = format_text.kind {
            let placeholder_count = text.split('_').count() - 1;

            if placeholder_count != inputs.len() {
                expander.compiler.add_error(
                    "wrong number of inputs to `format` text",
                    vec![Note::primary(
                        span,
                        format!(
                            "text contains {} placeholders, but {} inputs were provided",
                            placeholder_count,
                            inputs.len()
                        ),
                    )],
                );
            }
        } else {
            expander.compiler.add_error(
                "expected text",
                vec![Note::primary(
                    span,
                    "`instance` requires text containing `_` placeholders",
                )],
            );

            return Expression {
                span,
                kind: ExpressionKind::error(expander.compiler),
            };
        };

        Expression {
            span,
            kind: ExpressionKind::Annotate(
                Box::new(Expression {
                    span,
                    kind: ExpressionKind::External(
                        Box::new(Expression {
                            span,
                            kind: ExpressionKind::Text(InternedString::new("runtime")),
                        }),
                        Box::new(Expression {
                            span,
                            kind: ExpressionKind::Text(InternedString::new("format")),
                        }),
                        vec![
                            format_text,
                            Expression {
                                span,
                                kind: ExpressionKind::Annotate(
                                    Box::new(Expression {
                                        span,
                                        kind: ExpressionKind::List(vec![
                                            Expression {
                                                span,
                                                kind: ExpressionKind::Name(InternedString::new(
                                                    "list",
                                                )),
                                            },
                                            Expression {
                                                span,
                                                kind: ExpressionKind::Tuple(
                                                    inputs
                                                        .into_iter()
                                                        .map(|input| Expression {
                                                            span: input.span,
                                                            kind: ExpressionKind::List(vec![
                                                                Expression {
                                                                    span: input.span,
                                                                    kind: ExpressionKind::Name(
                                                                        InternedString::new("Show"),
                                                                    ),
                                                                },
                                                                input,
                                                            ]),
                                                        })
                                                        .collect(),
                                                ),
                                            },
                                        ]),
                                    }),
                                    Box::new(Expression {
                                        span,
                                        kind: ExpressionKind::List(vec![
                                            Expression {
                                                span,
                                                kind: ExpressionKind::Name(InternedString::new(
                                                    "List",
                                                )),
                                            },
                                            Expression {
                                                span,
                                                kind: ExpressionKind::Name(InternedString::new(
                                                    "Text",
                                                )),
                                            },
                                        ]),
                                    }),
                                ),
                            },
                        ],
                    ),
                }),
                Box::new(Expression {
                    span,
                    kind: ExpressionKind::Name(InternedString::new("Text")),
                }),
            ),
        }
    }
}
