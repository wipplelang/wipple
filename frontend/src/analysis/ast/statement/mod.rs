mod annotate;
mod assign;
mod expression;
mod instance;
mod type_function;
mod r#use;

pub use annotate::AnnotateStatement;
pub use assign::AssignStatement;
pub use expression::ExpressionStatement;
pub use instance::InstanceStatement;
pub use r#use::{UseStatement, UseStatementKind};
pub use type_function::TypeFunctionStatement;

use annotate::*;
use assign::*;
use expression::*;
use instance::*;
use r#use::*;
use type_function::*;

use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes,
    },
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[allow(clippy::large_enum_variant)]
    #[derive(Debug, Clone)]
    pub type Statement<StatementSyntaxContext> {
        non_terminal: {
            Assign,
            Annotate,
            TypeFunction,
            Instance,
            Use,
            Expression,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct StatementSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for StatementSyntaxContext {
    type Body = Statement;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        StatementSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    fn block_scope(&self, scope: ScopeId) -> ScopeId {
        self.ast_builder.child_scope(scope)
    }

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_block(span, statements, scope)
            .await
            .map(|expr| {
                ExpressionStatement {
                    expression: expr,
                    attributes: self.statement_attributes.unwrap().lock().clone(),
                }
                .into()
            })
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        let expr = parse::Expr::list(expr.span, vec![expr]);

        context.build_terminal(expr, scope).await.map(|expr| {
            ExpressionStatement {
                expression: expr,
                attributes: self.statement_attributes.unwrap().lock().clone(),
            }
            .into()
        })
    }
}
