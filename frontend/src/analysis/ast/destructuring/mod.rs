mod assign;

pub use assign::AssignDestructuringSyntax;

use assign::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    ScopeId,
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

impl NameDestructuring {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ListDestructuring {
    pub span: Span,
    pub names: Vec<Result<NameDestructuring, SyntaxError>>,
}

impl ListDestructuring {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone)]
pub struct DestructuringSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
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

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
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
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(
                span,
                "blocks may not be nested inside a destructuring pattern",
            )],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let names = exprs
                    .into_iter()
                    .map(|expr| match expr.kind {
                        parse::ExprKind::Name(name, _) => Ok(NameDestructuring {
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
                parse::ExprKind::Name(name, _) => Ok(NameDestructuring {
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
