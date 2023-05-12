mod function;
mod tuple;

pub use function::FunctionType;
pub use tuple::TupleType;

use function::*;
use tuple::*;

use crate::{
    ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use wipple_util::Shared;

syntax_group! {
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
pub struct PlaceholderType<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for PlaceholderType<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(PlaceholderType {
            span: Default::default(),
        })
    }
}

impl<D: Driver> PlaceholderType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct UnitType<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for UnitType<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(UnitType {
            span: Default::default(),
        })
    }
}

impl<D: Driver> UnitType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct NamedType<D: Driver> {
    pub span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub name_scope: D::Scope,
    pub parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NamedType<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NamedType {
            span: Default::default(),
            name_span: Default::default(),
            name: arbitrary::Arbitrary::arbitrary(u)?,
            name_scope: arbitrary::Arbitrary::arbitrary(u)?,
            parameters: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> NamedType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Clone)]
pub struct TypeSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for TypeSyntaxContext<D> {
    type Body = Type<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        TypeSyntaxContext {
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
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(span, "expected a type");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, mut exprs)) => {
                let (name_span, name, name_scope) = match exprs.next() {
                    Some(expr) => match expr.kind {
                        parse::ExprKind::Name(name, name_scope) => {
                            (expr.span, name, name_scope.unwrap_or(scope))
                        }
                        _ => {
                            self.ast_builder
                                .driver
                                .syntax_error(expr.span, "expected a type");

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
                    self.ast_builder
                        .driver
                        .syntax_error(expr.span, "expected a type");

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
