definitions! {
    mod assign;
}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use std::collections::HashSet;
use wipple_util::Shared;

syntax_group! {
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
pub struct NameDestructuring<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: HashSet<D::Scope>,
}

impl<D: Driver> NameDestructuring<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameDestructuring<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.name.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ListDestructuring<D: Driver> {
    pub span: D::Span,
    pub names: Vec<Result<NameDestructuring<D>, SyntaxError<D>>>,
}

impl<D: Driver> ListDestructuring<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ListDestructuring<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self
            .names
            .into_iter()
            .map(|result| Ok(result?.name.as_ref().to_string()))
            .collect::<Result<Vec<_>, _>>()?
            .join(" "))
    }
}

#[derive(Clone)]
pub struct DestructuringSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for DestructuringSyntaxContext<D> {
    type Body = Destructuring<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        DestructuringSyntaxContext {
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
        _scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder.driver.syntax_error(
            span,
            "blocks may not be nested inside a destructuring pattern",
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let names = exprs
                    .into_iter()
                    .map(|expr| match expr.kind {
                        parse::ExprKind::Name(name, scope) => Ok(NameDestructuring {
                            span: expr.span,
                            name,
                            scope: scope.unwrap_or_else(|| scope_set.lock().clone()),
                        }),
                        _ => {
                            self.ast_builder
                                .driver
                                .syntax_error(expr.span, "invalid destructuring pattern");

                            Err(self.ast_builder.syntax_error(expr.span))
                        }
                    })
                    .collect();

                Ok(ListDestructuring { span, names }.into())
            }
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, scope) => Ok(NameDestructuring {
                    span: expr.span,
                    name,
                    scope: scope.unwrap_or_else(|| scope_set.lock().clone()),
                }
                .into()),
                _ => {
                    self.ast_builder
                        .driver
                        .syntax_error(expr.span, "invalid destructuring pattern");

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
