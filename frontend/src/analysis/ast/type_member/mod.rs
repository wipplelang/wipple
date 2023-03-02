mod field;

pub use field::FieldTypeMember;

use field::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, Type, TypeSyntax, TypeSyntaxContext,
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
pub struct VariantTypeMember {
    pub span: SpanList,
    pub name_span: SpanList,
    pub name: InternedString,
    pub tys: Vec<Result<Type, SyntaxError>>,
}

impl VariantTypeMember {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Clone)]
pub struct TypeMemberSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for TypeMemberSyntaxContext {
    type Body = TypeMember;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        TypeMemberSyntaxContext {
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
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(span, "expected a field or variant")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, mut list)) => {
                let name_expr = match list.next() {
                    Some(expr) => expr,
                    None => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "expected variant")],
                        );

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let name = match name_expr.kind {
                    parse::ExprKind::Name(name, _) => name,
                    _ => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(name_expr.span, "expected variant")],
                        );

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
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "expected variant")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
