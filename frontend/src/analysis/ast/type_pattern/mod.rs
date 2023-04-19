mod default;
mod r#where;

pub use default::DefaultTypePattern;
pub use r#where::{WhereTypePattern, WhereTypePatternBound};

use default::*;
use r#where::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
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
    pub type TypePattern<TypePatternSyntaxContext> {
        non_terminal: {
            Default,
            Where,
        },
        terminal: {
            Name,
            List,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NameTypePattern {
    pub span: SpanList,
    pub name: InternedString,
}

impl NameTypePattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct ListTypePattern {
    pub span: SpanList,
    pub patterns: Vec<Result<TypePattern, SyntaxError>>,
}

impl ListTypePattern {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Clone)]
pub struct TypePatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for TypePatternSyntaxContext {
    type Body = TypePattern;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        TypePatternSyntaxContext {
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
            vec![Note::primary(span, "expected type parameter")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let patterns = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder
                            .build_expr::<TypePatternSyntax>(self.clone(), expr, scope)
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(ListTypePattern { span, patterns }.into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, _) => {
                    self.ast_builder.add_barrier(name, scope);

                    Ok(NameTypePattern {
                        span: expr.span,
                        name,
                    }
                    .into())
                }
                _ => {
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "expected type parameter")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
