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
    ScopeId,
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
            Unit,
            Name,
            Text,
            Number,
            Call,
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitExpression {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NameExpression {
    pub span: Span,
    pub name: InternedString,
    pub scope: ScopeId,
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
pub struct CallExpression {
    pub span: Span,
    pub function: Result<Box<Expression>, SyntaxError>,
    pub inputs: Vec<Result<Expression, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub span: Span,
    pub statements: Vec<Result<Statement, SyntaxError>>,
    pub scope: ScopeId,
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
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let scope = self.ast_builder.child_scope(scope);

        Ok(BlockExpression {
            span,
            statements: statements.collect(),
            scope,
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let mut exprs = exprs.into_iter();

                let function = match exprs.next() {
                    Some(expr) => {
                        self.ast_builder
                            .build_expr::<ExpressionSyntax>(self.clone(), expr, scope)
                            .await
                    }
                    None => return Ok(UnitExpression { span }.into()),
                };

                let inputs = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<ExpressionSyntax>(self.clone(), expr, scope)
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(CallExpression {
                    span,
                    function: function.map(Box::new),
                    inputs,
                }
                .into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name) => Ok(NameExpression {
                    span: expr.span,
                    name,
                    scope,
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
