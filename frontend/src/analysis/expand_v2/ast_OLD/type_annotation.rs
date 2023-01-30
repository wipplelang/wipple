use crate::{
    analysis::expand_v2::{
        syntax::{SyntaxExpression, SyntaxExpressionKind},
        Expander, Scope, Visitor,
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
pub struct TypeAnnotation {
    pub span: Span,
    pub kind: TypeAnnotationKind,
}

#[derive(Debug, Clone)]
pub enum TypeAnnotationKind {
    Error(Backtrace),
    Placeholder,
    Named(InternedString, Vec<TypeAnnotation>),
    Function(Box<TypeAnnotation>, Box<TypeAnnotation>),
    Tuple(Vec<TypeAnnotation>),
}

impl TypeAnnotationKind {
    fn error(compiler: &Compiler) -> Self {
        TypeAnnotationKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct TypeParameter {
    pub span: Span,
    pub name: InternedString,
}

impl<'a, 'l> Expander<'a, 'l> {
    #[async_recursion]
    async fn expand_type_annotation(
        &'a self,
        expr: SyntaxExpression,
        scope: &'a Scope<'_>,
    ) -> TypeAnnotation {
        let proxy = TypeAnnotationProxy { expander: self };
        proxy.expand(expr, scope).await
    }
}

#[derive(Clone, Copy)]
struct TypeAnnotationProxy<'a, 'l> {
    expander: &'a Expander<'a, 'l>,
}

#[async_trait]
impl<'a, 'l> Visitor<'a> for TypeAnnotationProxy<'a, 'l> {
    type Output = TypeAnnotation;

    fn error(self, span: Span, trace: Backtrace) -> Self::Output {
        TypeAnnotation {
            span,
            kind: TypeAnnotationKind::Error(trace),
        }
    }

    fn empty(self, span: Span) -> Self::Output {
        TypeAnnotation {
            span,
            kind: TypeAnnotationKind::Tuple(Vec::new()),
        }
    }

    async fn expand(self, expr: SyntaxExpression, scope: &'a Scope<'a>) -> Self::Output {
        match expr.kind {
            SyntaxExpressionKind::Error(trace) => TypeAnnotation {
                span: expr.span,
                kind: TypeAnnotationKind::Error(trace),
            },
            SyntaxExpressionKind::Underscore => TypeAnnotation {
                span: expr.span,
                kind: TypeAnnotationKind::Placeholder,
            },
            SyntaxExpressionKind::Name(name) => TypeAnnotation {
                span: expr.span,
                kind: TypeAnnotationKind::Named(name, Vec::new()),
            },
            SyntaxExpressionKind::List(exprs) => {
                self.expander
                    .expand_list(expr.span, exprs, self, scope)
                    .await
            }
            _ => {
                self.expander.compiler.add_error(
                    "expected type",
                    vec![Note::primary(expr.span, "this is not a type")],
                );

                TypeAnnotation {
                    span: expr.span,
                    kind: TypeAnnotationKind::error(self.expander.compiler),
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
        let ty_name = match first.kind {
            SyntaxExpressionKind::Name(name) => name,
            _ => {
                self.expander.compiler.add_error(
                    "expected type name",
                    vec![Note::primary(first.span, "try removing this")],
                );

                return TypeAnnotation {
                    span,
                    kind: TypeAnnotationKind::error(self.expander.compiler),
                };
            }
        };

        let params = stream::iter(rest)
            .then(|node| self.expand(node, scope))
            .collect::<Vec<_>>()
            .await;

        TypeAnnotation {
            span,
            kind: TypeAnnotationKind::Named(ty_name, params),
        }
    }
}
