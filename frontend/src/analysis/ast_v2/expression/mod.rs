mod annotate;
mod end;
mod external;
mod format;
mod function;
mod tuple;
mod when;

pub use annotate::AnnotateExpression;
pub use end::EndExpression;
pub use external::ExternalExpression;
pub use format::FormatExpression;
pub use function::FunctionExpression;
pub use tuple::TupleExpression;
pub use when::WhenExpression;

use annotate::*;
use end::*;
use external::*;
use format::*;
use function::*;
use tuple::*;
use when::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Statement, StatementAttributes, StatementSyntax,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
};
use async_trait::async_trait;
use futures::{stream, StreamExt};

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Expression<ExpressionSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
            Annotate,
            End,
            External,
            Format,
            When,
        },
        terminal: {
            Name,
            Text,
            Number,
            List,
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NameExpression {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct TextExpression {
    pub span: Span,
    pub text: InternedString,
}

#[derive(Debug, Clone)]
pub struct NumberExpression {
    pub span: Span,
    pub number: InternedString,
}

#[derive(Debug, Clone)]
pub struct ListExpression {
    pub span: Span,
    pub exprs: Vec<Result<Expression, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub span: Span,
    pub statements: Vec<Result<Statement, SyntaxError>>,
}

#[derive(Clone)]
pub struct ExpressionSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for ExpressionSyntaxContext {
    type Body = Expression;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        ExpressionSyntaxContext {
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
        Ok(BlockExpression {
            span,
            statements: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let exprs = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<ExpressionSyntax>(self.clone(), expr)
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(ListExpression { span, exprs }.into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name) => Ok(NameExpression {
                    span: expr.span,
                    name,
                }
                .into()),
                parse::ExprKind::Text(text) => Ok(TextExpression {
                    span: expr.span,
                    text,
                }
                .into()),
                parse::ExprKind::Number(number) => Ok(NumberExpression {
                    span: expr.span,
                    number,
                }
                .into()),
                _ => {
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "expected expression")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}

impl FileBodySyntaxContext for ExpressionSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
