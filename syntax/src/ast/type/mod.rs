definitions! {
    mod function;
    mod tuple;
}

use crate::ScopeSet;
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

impl<D: Driver> PlaceholderType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for PlaceholderType<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("_"))
    }
}

#[derive(Debug, Clone)]
pub struct UnitType<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnitType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnitType<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("()"))
    }
}

#[derive(Debug, Clone)]
pub struct NamedType<D: Driver> {
    pub span: D::Span,
    pub name_span: D::Span,
    pub name: D::InternedString,
    pub name_scope_set: ScopeSet<D::Scope>,
    pub parameters: Vec<Result<Type<D>, SyntaxError<D>>>,
}

impl<D: Driver> NamedType<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NamedType<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({}{})",
            self.name.as_ref(),
            self.parameters
                .into_iter()
                .map(|ty| Ok(format!(" {}", ty?.format()?)))
                .collect::<Result<String, _>>()?
        ))
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
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(span, "expected a type");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, mut exprs)) => {
                let (name_span, name, name_scope_set) = match exprs.next() {
                    Some(expr) => match expr.kind {
                        parse::ExprKind::Name(name, name_scope) => (
                            expr.span,
                            name,
                            name_scope.unwrap_or_else(|| scope_set.lock().clone()),
                        ),
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
                        self.ast_builder.build_expr::<TypeSyntax>(
                            self.clone(),
                            expr,
                            scope_set.clone(),
                        )
                    })
                    .collect()
                    .await;

                Ok(NamedType {
                    name_span,
                    name,
                    name_scope_set,
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
                    name_scope_set: name_scope.unwrap_or_else(|| scope_set.lock().clone()),
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
