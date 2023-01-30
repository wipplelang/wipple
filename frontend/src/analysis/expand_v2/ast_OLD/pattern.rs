use crate::{
    analysis::expand_v2::{
        syntax::{BuiltinSyntax, SyntaxExpression, SyntaxExpressionKind},
        ExpandOperatorsResult, Expander, Scope, Syntax, TypeAnnotation, Visitor,
    },
    diagnostics::Note,
    helpers::{Backtrace, InternedString},
    parse::Span,
    Compiler,
};
use async_recursion::async_recursion;
use async_trait::async_trait;
use futures::{stream, StreamExt};

#[derive(Debug, Clone)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    Error(Backtrace),
    Wildcard,
    Number(InternedString),
    Text(InternedString),
    Name(InternedString),
    Destructure(Vec<(InternedString, Pattern)>),
    Variant((Span, InternedString), Vec<Pattern>),
    Annotate(Box<Pattern>, TypeAnnotation),
    Or(Box<Pattern>, Box<Pattern>),
    Where(Box<Pattern>, Box<Pattern>),
    Tuple(Vec<Pattern>),
}

impl PatternKind {
    fn error(compiler: &Compiler) -> Self {
        PatternKind::Error(compiler.backtrace())
    }
}

impl<'a, 'l> Expander<'a, 'l> {
    #[async_recursion]
    async fn expand_pattern(&'a self, expr: SyntaxExpression, scope: &'a Scope<'_>) -> Pattern {
        let proxy = PatternProxy { expander: self };
        proxy.expand(expr, scope).await
    }
}

#[derive(Clone, Copy)]
struct PatternProxy<'a, 'l> {
    expander: &'a Expander<'a, 'l>,
}

#[async_trait]
impl<'a, 'l> Visitor<'a> for PatternProxy<'a, 'l> {
    type Output = Pattern;

    fn error(self, span: Span, trace: Backtrace) -> Self::Output {
        Pattern {
            span,
            kind: PatternKind::Error(trace),
        }
    }

    fn empty(self, span: Span) -> Self::Output {
        Pattern {
            span,
            kind: PatternKind::Tuple(Vec::new()),
        }
    }

    async fn expand(self, expr: SyntaxExpression, scope: &'a Scope<'a>) -> Self::Output {
        match expr.kind {
            SyntaxExpressionKind::Error(trace) => Pattern {
                span: expr.span,
                kind: PatternKind::Error(trace),
            },
            SyntaxExpressionKind::Underscore => Pattern {
                span: expr.span,
                kind: PatternKind::Wildcard,
            },
            SyntaxExpressionKind::Name(name) => Pattern {
                span: expr.span,
                kind: PatternKind::Name(name),
            },
            SyntaxExpressionKind::Text(text) => Pattern {
                span: expr.span,
                kind: PatternKind::Text(text),
            },
            SyntaxExpressionKind::Number(number) => Pattern {
                span: expr.span,
                kind: PatternKind::Number(number),
            },
            SyntaxExpressionKind::List(exprs) => {
                self.expander
                    .expand_list(expr.span, exprs, self, scope)
                    .await
            }
            SyntaxExpressionKind::Block(statements) => {
                let patterns = stream::iter(statements)
                    .then(|statement| {
                        Box::pin(async {
                            match self
                                .expander
                                .expand_operators(expr.span, statement.exprs, scope)
                            {
                                ExpandOperatorsResult::Error(_) => Box::new(std::iter::empty()),
                                ExpandOperatorsResult::Single(expr) => match expr.kind {
                                    SyntaxExpressionKind::Error(_) => Box::new(std::iter::empty())
                                        as Box<dyn Iterator<Item = _> + Send>,
                                    SyntaxExpressionKind::Name(name) => {
                                        Box::new(std::iter::once((
                                            name,
                                            Pattern {
                                                span: expr.span,
                                                kind: PatternKind::Name(name),
                                            },
                                        )))
                                    }
                                    _ => {
                                        self.expander.compiler.add_error(
                                            "invalid pattern in destructuring pattern",
                                            vec![Note::primary(expr.span, "expected name here")],
                                        );

                                        Box::new(std::iter::empty())
                                    }
                                },
                                ExpandOperatorsResult::List(first, rest) => {
                                    Box::new(std::iter::once(first).chain(rest).filter_map(
                                        |expr| match expr.kind {
                                            SyntaxExpressionKind::Error(_) => None,
                                            SyntaxExpressionKind::Name(name) => Some((
                                                name,
                                                Pattern {
                                                    span: expr.span,
                                                    kind: PatternKind::Name(name),
                                                },
                                            )),
                                            _ => {
                                                self.expander.compiler.add_error(
                                                    "invalid pattern in destructuring pattern",
                                                    vec![Note::primary(
                                                        expr.span,
                                                        "expected name here",
                                                    )],
                                                );

                                                None
                                            }
                                        },
                                    ))
                                }
                                ExpandOperatorsResult::Operator(
                                    _,
                                    Syntax::Builtin(BuiltinSyntax::Assign(_)),
                                    left,
                                    right,
                                ) => {
                                    let name = match left.kind {
                                        SyntaxExpressionKind::Name(name) => name,
                                        _ => {
                                            self.expander.compiler.add_error(
                                                "invalid pattern in destructuring pattern",
                                                vec![Note::primary(
                                                    left.span,
                                                    "expected name here",
                                                )],
                                            );

                                            return Box::new(std::iter::empty())
                                                as Box<dyn Iterator<Item = _> + Send>;
                                        }
                                    };

                                    let pattern = self.expand(right, scope).await;

                                    Box::new(std::iter::once((name, pattern)))
                                }
                                _ => {
                                    self.expander.compiler.add_error(
                                        "invalid pattern in destructuring pattern",
                                        vec![Note::primary(expr.span, "try removing this")],
                                    );

                                    Box::new(std::iter::empty())
                                }
                            }
                        })
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>();

                Pattern {
                    span: expr.span,
                    kind: PatternKind::Destructure(patterns),
                }
            }
            _ => {
                self.expander.compiler.add_error(
                    "expected pattern",
                    vec![Note::primary(expr.span, "this is not a pattern")],
                );

                Pattern {
                    span: expr.span,
                    kind: PatternKind::error(self.expander.compiler),
                }
            }
        }
    }

    async fn reduce(
        self,
        span: Span,
        first: SyntaxExpression,
        rest: impl Iterator<Item = SyntaxExpression> + Send,
        scope: &'a Scope<'a>,
    ) -> Self::Output {
        let variant_name = match first.kind {
            SyntaxExpressionKind::Name(name) => name,
            _ => {
                self.expander.compiler.add_error(
                    "expected variant name",
                    vec![Note::primary(
                        span,
                        "only variants may be used in this kind of pattern",
                    )],
                );

                return Pattern {
                    span,
                    kind: PatternKind::error(self.expander.compiler),
                };
            }
        };

        let patterns = stream::iter(rest)
            .then(|node| self.expand(node, scope))
            .collect::<Vec<_>>()
            .await;

        Pattern {
            span,
            kind: PatternKind::Variant((first.span, variant_name), patterns),
        }
    }
}
