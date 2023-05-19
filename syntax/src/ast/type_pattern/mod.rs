definitions! {
    mod default;
    mod r#where;
}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver, File,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use wipple_util::Shared;

syntax_group! {
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
pub struct NameTypePattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NameTypePattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NameTypePattern {
            span: Default::default(),
            name: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> NameTypePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameTypePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("{}", self.name.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct ListTypePattern<D: Driver> {
    pub span: D::Span,
    pub patterns: Vec<Result<TypePattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> Format<D> for ListTypePattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({})",
            self.patterns
                .into_iter()
                .map(|pattern| pattern?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for ListTypePattern<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ListTypePattern {
            span: Default::default(),
            patterns: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> ListTypePattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Clone)]
pub struct TypePatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for TypePatternSyntaxContext<D> {
    type Body = TypePattern<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        TypePatternSyntaxContext {
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
            .syntax_error(span, "expected type parameter");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
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
                    self.ast_builder.file.add_barrier(name.clone(), scope);

                    Ok(NameTypePattern {
                        span: expr.span,
                        name,
                    }
                    .into())
                }
                _ => {
                    self.ast_builder
                        .driver
                        .syntax_error(expr.span, "expected type parameter");

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}
