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
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax, StatementAttributes,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, SpanList},
    ScopeId,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};

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
pub struct NamePattern {
    pub span: SpanList,
    pub name: InternedString,
}

impl NamePattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct TextPattern {
    pub span: SpanList,
    pub text: InternedString,
}

impl TextPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct NumberPattern {
    pub span: SpanList,
    pub number: InternedString,
}

impl NumberPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct UnitPattern {
    pub span: SpanList,
}

impl UnitPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct VariantPattern {
    pub span: SpanList,
    pub name_span: SpanList,
    pub name: InternedString,
    pub values: Vec<Result<Pattern, SyntaxError>>,
}

impl VariantPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct DestructurePattern {
    pub span: SpanList,
    pub destructurings: Vec<Result<Destructuring, SyntaxError>>,
}

impl DestructurePattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct WildcardPattern {
    pub span: SpanList,
}

impl WildcardPattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
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

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    async fn build_block(
        self,
        span: parse::SpanList,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Ok(DestructurePattern {
            span,
            destructurings: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.kind {
            parse::ExprKind::Name(name, _) => {
                self.ast_builder.add_barrier(name, scope);

                Ok(NamePattern {
                    span: expr.span,
                    name,
                }
                .into())
            }
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
                let (span, mut exprs) = expr.try_into_list_exprs().unwrap();

                let name_expr = match exprs.next() {
                    Some(expr) => expr,
                    None => return Ok(UnitPattern { span }.into()),
                };

                let name = match name_expr.kind {
                    parse::ExprKind::Name(name, _) => name,
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
                self.ast_builder.compiler.add_error(
                    "syntax error",
                    vec![Note::primary(expr.span, "expected expression")],
                );

                Err(self.ast_builder.syntax_error(expr.span))
            }
        }
    }
}
