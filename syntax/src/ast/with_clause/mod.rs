mod assign;

pub use assign::AssignWithClause;

use assign::*;

use crate::{
    ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type WithClause<WithClauseSyntaxContext> {
        non_terminal: {
            Assign,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct WithClauseSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for WithClauseSyntaxContext<D> {
    type Body = WithClause<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        WithClauseSyntaxContext {
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
            .syntax_error(span, "expected a variable assignment");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected a variable assignment");

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
