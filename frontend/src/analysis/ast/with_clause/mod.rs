mod assign;

pub use assign::AssignWithClause;

use assign::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    diagnostics::Note,
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type WithClause<WithClauseSyntaxContext> {
        non_terminal: {
            Assign,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct WithClauseSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for WithClauseSyntaxContext {
    type Body = WithClause;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        WithClauseSyntaxContext {
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
            vec![Note::primary(span, "expected a function")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "expected a function")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
