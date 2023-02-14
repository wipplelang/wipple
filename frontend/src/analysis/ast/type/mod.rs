mod function;
mod tuple;

pub use function::FunctionType;
pub use tuple::TupleType;

use function::*;
use tuple::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
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
    pub type Type<TypeSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
        },
        terminal: {
            Placeholder,
            Unit,
            Named,
        },
    }
}

#[derive(Debug, Clone)]
pub struct PlaceholderType {
    pub span: Span,
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
    pub name_scope: ScopeId,
    pub parameters: Vec<Result<Type, SyntaxError>>,
}

#[derive(Clone)]
pub struct TypeSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
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
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder
            .compiler
            .add_error("syntax error", vec![Note::primary(span, "expected a type")]);

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let mut exprs = exprs.into_iter();

                let (name_span, name, name_scope) = match exprs.next() {
                    Some(expr) => match expr.kind {
                        parse::ExprKind::Name(name, name_scope) => {
                            (expr.span, name, name_scope.unwrap_or(scope))
                        }
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
                            .build_expr::<TypeSyntax>(self.clone(), expr, scope)
                    })
                    .collect()
                    .await;

                Ok(NamedType {
                    name_span,
                    name,
                    name_scope,
                    span,
                    parameters,
                }
                .into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, name_scope) => Ok(NamedType {
                    span: expr.span,
                    name_span: expr.span,
                    name,
                    name_scope: name_scope.unwrap_or(scope),
                    parameters: Vec::new(),
                }
                .into()),
                parse::ExprKind::Underscore => Ok(PlaceholderType { span: expr.span }.into()),
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
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
