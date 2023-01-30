use crate::{
    analysis::expand_v2::{
        syntax::{SyntaxExpression, SyntaxExpressionKind},
        Expander, Pattern, Scope, Statement, TypeAnnotation, Visitor,
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
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Error(Backtrace),
    Name(InternedString),
    Number(InternedString),
    Text(InternedString),
    Block(Vec<Statement>),
    End(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Function(Pattern, Box<Expression>),
    When(Box<Expression>, Vec<Arm>),
    External(InternedString, InternedString, Vec<Expression>),
    Annotate(Box<Expression>, TypeAnnotation),
    Tuple(Vec<Expression>),
}

impl ExpressionKind {
    fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expression,
}

impl<'a, 'l> Expander<'a, 'l> {
    #[async_recursion]
    async fn expand_expr(&'a self, expr: SyntaxExpression, scope: &'a Scope<'_>) -> Expression {
        let proxy = ExpressionProxy { expander: self };
        proxy.expand(expr, scope).await
    }
}

#[derive(Clone, Copy)]
struct ExpressionProxy<'a, 'l> {
    expander: &'a Expander<'a, 'l>,
}

#[async_trait]
impl<'a, 'l> Visitor<'a> for ExpressionProxy<'a, 'l> {
    type Output = Expression;

    fn error(self, span: Span, trace: Backtrace) -> Self::Output {
        Expression {
            span,
            kind: ExpressionKind::Error(trace),
        }
    }

    fn empty(self, span: Span) -> Self::Output {
        Expression {
            span,
            kind: ExpressionKind::Tuple(Vec::new()),
        }
    }

    async fn expand(self, expr: SyntaxExpression, scope: &'a Scope<'a>) -> Self::Output {
        match expr.kind {
            SyntaxExpressionKind::Error(trace) => Expression {
                span: expr.span,
                kind: ExpressionKind::Error(trace),
            },
            SyntaxExpressionKind::Name(name) => Expression {
                span: expr.span,
                kind: ExpressionKind::Name(name),
            },
            SyntaxExpressionKind::Text(text) => Expression {
                span: expr.span,
                kind: ExpressionKind::Text(text),
            },
            SyntaxExpressionKind::Number(number) => Expression {
                span: expr.span,
                kind: ExpressionKind::Number(number),
            },
            SyntaxExpressionKind::List(exprs) => {
                self.expander
                    .expand_list(expr.span, exprs, self, scope)
                    .await
            }
            SyntaxExpressionKind::Block(statements) => Expression {
                span: expr.span,
                kind: ExpressionKind::Block(
                    stream::iter(statements)
                        .then(|statement| self.expander.expand_statement(statement, scope))
                        .collect()
                        .await,
                ),
            },
            _ => {
                self.expander.compiler.add_error(
                    "expected expression",
                    vec![Note::primary(expr.span, "this is not an expression")],
                );

                Expression {
                    span: expr.span,
                    kind: ExpressionKind::error(self.expander.compiler),
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
        let func = self.expand(first, scope).await;

        let inputs = stream::iter(rest)
            .then(|expr| self.expand(expr, scope))
            .collect()
            .await;

        Expression {
            span,
            kind: ExpressionKind::Call(Box::new(func), inputs),
        }
    }
}
