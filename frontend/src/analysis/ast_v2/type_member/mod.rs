mod field;

pub use field::FieldTypeMember;

use field::*;

use crate::{
    analysis::ast_v2::{
        syntax::{ErrorSyntax, FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Type, TypeSyntax, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
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
    pub span: Span,
    pub name: InternedString,
    pub tys: Vec<Result<Type, SyntaxError>>,
}

#[derive(Clone)]
pub struct TypeMemberSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
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
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(span, "expected a field or variant")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, list)) => {
                let mut list = list.into_iter();

                let name = match list.next() {
                    Some(expr) => expr,
                    None => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(span, "expected variant")],
                        );

                        return Err(self.ast_builder.syntax_error(span));
                    }
                };

                let name = match name.kind {
                    parse::ExprKind::Name(name) => name,
                    _ => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(name.span, "expected variant")],
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
                        )
                    })
                    .collect()
                    .await;

                Ok(VariantTypeMember { span, name, tys }.into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name) => Ok(VariantTypeMember {
                    span: expr.span,
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

impl FileBodySyntaxContext for TypeMemberSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
