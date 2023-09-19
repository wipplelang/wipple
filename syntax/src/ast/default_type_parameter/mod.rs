definitions! {
    mod infer;
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
use wipple_util::Shared;

syntax_group! {
    pub type DefaultTypeParameter<DefaultTypeParameterSyntaxContext> {
        non_terminal: {
            Infer,
        },
        terminal: {
            Name,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NameDefaultTypeParameter<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: ScopeSet<D::Scope>,
}

impl<D: Driver> NameDefaultTypeParameter<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameDefaultTypeParameter<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.name.as_ref().to_string())
    }
}

#[derive(Clone)]
pub struct DefaultTypeParameterSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for DefaultTypeParameterSyntaxContext<D> {
    type Body = DefaultTypeParameter<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        DefaultTypeParameterSyntaxContext {
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
            .syntax_error(span, "expected type parameter");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.kind {
            parse::ExprKind::Name(name, _) => Ok(NameDefaultTypeParameter {
                span: expr.span,
                name,
                scope: scope_set.lock().clone(),
            }
            .into()),
            _ => {
                self.ast_builder
                    .driver
                    .syntax_error(expr.span, "expected type parameter");

                Err(self.ast_builder.syntax_error(expr.span))
            }
        }
    }
}
