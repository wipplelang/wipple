mod function;
mod tuple;

pub use function::FunctionType;
pub use tuple::TupleType;

use function::*;
use tuple::*;

use crate::{
    analysis::ast_v2::{
        syntax::{ErrorSyntax, FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
};
use async_trait::async_trait;
use futures::{stream, StreamExt};

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Type<TypeSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
        },
        terminal: {
            Unit,
            Named,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitType {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NamedType {
    pub span: Span,
    pub name_span: Span,
    pub name: InternedString,
    pub parameters: Vec<Result<Type, SyntaxError>>,
}

#[derive(Clone)]
pub struct TypeSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for TypeSyntaxContext {
    type Body = Type;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        TypeSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder
            .compiler
            .add_error("syntax error", vec![Note::primary(span, "expected a type")]);

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let mut exprs = exprs.into_iter();

                let (name_span, name) = match exprs.next() {
                    Some(expr) => match expr.kind {
                        parse::ExprKind::Name(name) => (expr.span, name),
                        _ => {
                            self.ast_builder.compiler.add_error(
                                "syntax error",
                                vec![Note::primary(expr.span, "expected a type")],
                            );

                            return Err(self.ast_builder.syntax_error(expr.span));
                        }
                    },
                    None => return Ok(UnitType { span }.into()),
                };

                let parameters = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<TypeSyntax>(self.clone(), expr)
                    })
                    .collect()
                    .await;

                Ok(NamedType {
                    name_span,
                    name,
                    span,
                    parameters,
                }
                .into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name) => Ok(NamedType {
                    span: expr.span,
                    name_span: expr.span,
                    name,
                    parameters: Vec::new(),
                }
                .into()),
                _ => {
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "expected a type")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}

impl FileBodySyntaxContext for TypeSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
