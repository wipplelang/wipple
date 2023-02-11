mod annotate;
mod tuple;
mod r#where;

pub use annotate::AnnotatePattern;
pub use r#where::WherePattern;
pub use tuple::TuplePattern;

use annotate::*;
use r#where::*;
use tuple::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax, StatementAttributes,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
};
use async_trait::async_trait;
use futures::{stream, StreamExt};

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Pattern<PatternSyntaxContext> {
        non_terminal: {
            Tuple,
            Annotate,
            Where,
        },
        terminal: {
            Name,
            Text,
            Number,
            Unit,
            Variant,
            Destructure,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NamePattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct TextPattern {
    pub span: Span,
    pub text: InternedString,
}

#[derive(Debug, Clone)]
pub struct NumberPattern {
    pub span: Span,
    pub number: InternedString,
}

#[derive(Debug, Clone)]
pub struct UnitPattern {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariantPattern {
    pub span: Span,
    pub name_span: Span,
    pub name: InternedString,
    pub values: Vec<Result<Pattern, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct DestructurePattern {
    pub span: Span,
    pub destructurings: Vec<Result<Destructuring, SyntaxError>>,
}

#[derive(Clone)]
pub struct PatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for PatternSyntaxContext {
    type Body = Pattern;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        PatternSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
    ) -> Result<Self::Body, SyntaxError> {
        Ok(DestructurePattern {
            span,
            destructurings: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        match expr.kind {
            parse::ExprKind::Name(name) => Ok(NamePattern {
                span: expr.span,
                name,
            }
            .into()),
            parse::ExprKind::Text(text) => Ok(TextPattern {
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
                let (span, exprs) = expr.try_into_list_exprs().unwrap();
                let mut exprs = exprs.into_iter();

                let name_expr = match exprs.next() {
                    Some(expr) => expr,
                    None => return Ok(UnitPattern { span }.into()),
                };

                let name = match name_expr.kind {
                    parse::ExprKind::Name(name) => name,
                    _ => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(name_expr.span, "expected name")],
                        );

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let values = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<PatternSyntax>(self.clone(), expr)
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
            _ => {
                self.ast_builder.compiler.add_error(
                    "syntax error",
                    vec![Note::primary(expr.span, "expected expression")],
                );

                Err(self.ast_builder.syntax_error(expr.span))
            }
        }
    }
}

impl FileBodySyntaxContext for PatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
