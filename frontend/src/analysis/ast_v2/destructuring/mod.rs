mod assign;

pub use assign::AssignDestructuringSyntax;

use assign::*;

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

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Destructuring<DestructuringSyntaxContext> {
        non_terminal: {
            Assign,
        },
        terminal: {
            Name,
            List,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NameDestructuring {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct ListDestructuring {
    pub span: Span,
    pub names: Vec<Result<NameDestructuring, SyntaxError>>,
}

#[derive(Clone)]
pub struct DestructuringSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for DestructuringSyntaxContext {
    type Body = Destructuring;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        DestructuringSyntaxContext {
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
            vec![Note::primary(
                span,
                "blocks may not be nested inside a destructuring pattern",
            )],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let names = exprs
                    .into_iter()
                    .map(|expr| match expr.kind {
                        parse::ExprKind::Name(name) => Ok(NameDestructuring {
                            span: expr.span,
                            name,
                        }),
                        _ => {
                            self.ast_builder.compiler.add_error(
                                "syntax error",
                                vec![Note::primary(expr.span, "invalid destructuring pattern")],
                            );

                            Err(self.ast_builder.syntax_error(expr.span))
                        }
                    })
                    .collect();

                Ok(ListDestructuring { span, names }.into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name) => Ok(NameDestructuring {
                    span: expr.span,
                    name,
                }
                .into()),
                _ => {
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "invalid destructuring pattern")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}

impl FileBodySyntaxContext for DestructuringSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
