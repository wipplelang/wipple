mod annotate;
mod or;
mod tuple;

pub use annotate::AnnotatePattern;
pub use or::OrPattern;
pub use tuple::TuplePattern;

use annotate::*;
use or::*;
use tuple::*;

use crate::{
    ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax, StatementAttributes,
    },
    parse, Driver, File,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use wipple_util::Shared;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Pattern<PatternSyntaxContext> {
        non_terminal: {
            Tuple,
            Annotate,
            Or,
        },
        terminal: {
            Name,
            Text,
            Number,
            Unit,
            Variant,
            Destructure,
            Wildcard,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NamePattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: D::Scope,
}

impl<D: Driver> NamePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct TextPattern<D: Driver> {
    pub span: D::Span,
    pub text: D::InternedString,
}

impl<D: Driver> TextPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct NumberPattern<D: Driver> {
    pub span: D::Span,
    pub number: D::InternedString,
}

impl<D: Driver> NumberPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct UnitPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnitPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct VariantPattern<D: Driver> {
    pub span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub values: Vec<Result<Pattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> VariantPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct DestructurePattern<D: Driver> {
    pub span: D::Span,
    pub destructurings: Vec<Result<Destructuring<D>, SyntaxError<D>>>,
}

impl<D: Driver> DestructurePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct WildcardPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> WildcardPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Clone)]
pub struct PatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for PatternSyntaxContext<D> {
    type Body = Pattern<D>;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        PatternSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(DestructurePattern {
            span,
            destructurings: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.kind {
            parse::ExprKind::Name(name, _sc) => {
                self.ast_builder.file.add_barrier(name, scope);

                Ok(NamePattern {
                    span: expr.span,
                    name,
                    scope,
                }
                .into())
            }
            parse::ExprKind::Text(text, _) => Ok(TextPattern {
                span: expr.span,
                text,
            }
            .into()),
            parse::ExprKind::Number(number) => Ok(NumberPattern {
                span: expr.span,
                number,
            }
            .into()),
            parse::ExprKind::List(_) => {
                let (span, mut exprs) = expr.try_into_list_exprs().unwrap();

                let name_expr = match exprs.next() {
                    Some(expr) => expr,
                    None => return Ok(UnitPattern { span }.into()),
                };

                let name = match name_expr.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        self.ast_builder
                            .driver
                            .syntax_error(name_expr.span, "expected name");

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let values = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<PatternSyntax>(self.clone(), expr, scope)
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(VariantPattern {
                    span,
                    name_span: name_expr.span,
                    name,
                    values,
                }
                .into())
            }
            parse::ExprKind::Underscore => Ok(WildcardPattern { span: expr.span }.into()),
            _ => {
                self.ast_builder
                    .driver
                    .syntax_error(expr.span, "expected expression");

                Err(self.ast_builder.syntax_error(expr.span))
            }
        }
    }
}
