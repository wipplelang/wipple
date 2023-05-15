mod assign;

pub use assign::AssignDestructuringSyntax;

use assign::*;

use crate::{
    ast::{
        format::Format,
        macros::syntax_group,
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
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
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NameDestructuring<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NameDestructuring {
            span: Default::default(),
            name: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
}

impl<D: Driver> NameDestructuring<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameDestructuring<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("{}", self.name.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct ListDestructuring<D: Driver> {
    pub span: D::Span,
    pub names: Vec<Result<NameDestructuring<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for ListDestructuring<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ListDestructuring {
            span: Default::default(),
            names: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
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
        _scope: D::Scope,
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
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let names = exprs
                    .into_iter()
                    .map(|expr| match expr.kind {
                        parse::ExprKind::Name(name, name_scope) => Ok(NameDestructuring {
                            span: expr.span,
                            name,
                            scope: name_scope.unwrap_or(scope),
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
                parse::ExprKind::Name(name, name_scope) => Ok(NameDestructuring {
                    span: expr.span,
                    name,
                    scope: name_scope.unwrap_or(scope),
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
