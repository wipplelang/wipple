mod annotate;
mod assign;
mod instance;
mod r#use;

pub use annotate::AnnotateStatement;
pub use assign::AssignStatement;
pub use instance::InstanceStatement;
pub use r#use::{UseStatement, UseStatementKind};

use annotate::*;
use assign::*;
use instance::*;
use r#use::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Expression, ExpressionSyntaxContext, StatementAttributes,
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
            Annotate,
            Assign,
            Instance,
            Use,
        },
        terminal: {
            Expression,
        },
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub attributes: StatementAttributes,
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

        context.build_terminal(expr, scope).await.map(|expr| {
            ExpressionStatement {
                expression: expr,
                attributes: self.statement_attributes.unwrap().lock().clone(),
            }
            .into()
        })
    }
}

impl FileBodySyntaxContext for StatementSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
