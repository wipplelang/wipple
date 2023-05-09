mod field;

pub use field::FieldTypeMember;

use field::*;

use crate::{
    ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
    },
    parse, Driver,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use wipple_util::Shared;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type TypeMember<TypeMemberSyntaxContext> {
        non_terminal: {
            Field,
        },
        terminal: {
            Variant,
        },
    }
}

#[derive(Debug, Clone)]
pub struct VariantTypeMember<D: Driver> {
    pub span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub tys: Vec<Result<Type<D>, SyntaxError<D>>>,
}

impl<D: Driver> VariantTypeMember<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Clone)]
pub struct TypeMemberSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for TypeMemberSyntaxContext<D> {
    type Body = TypeMember<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        TypeMemberSyntaxContext {
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
            .syntax_error(span, "expected a field or variant");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, mut list)) => {
                let name_expr = match list.next() {
                    Some(expr) => expr,
                    None => {
                        self.ast_builder
                            .driver
                            .syntax_error(span, "expected variant");

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let name = match name_expr.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        self.ast_builder
                            .driver
                            .syntax_error(name_expr.span, "expected variant");

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let tys = stream::iter(list)
                    .then(|expr| {
                        self.ast_builder.build_expr::<TypeSyntax>(
                            TypeSyntaxContext::new(self.ast_builder.clone())
                                .with_statement_attributes(
                                    self.statement_attributes.as_ref().unwrap().clone(),
                                ),
                            expr,
                            scope,
                        )
                    })
                    .collect()
                    .await;

                Ok(VariantTypeMember {
                    span,
                    name_span: name_expr.span,
                    name,
                    tys,
                }
                .into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, _) => Ok(VariantTypeMember {
                    span: expr.span,
                    name_span: expr.span,
                    name,
                    tys: Vec::new(),
                }
                .into()),
                _ => {
                    self.ast_builder
                        .driver
                        .syntax_error(expr.span, "expected variant");

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
