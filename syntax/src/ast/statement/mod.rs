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
    ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes,
    },
    parse, Driver, File,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    #[allow(clippy::large_enum_variant)]
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
pub struct StatementSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for StatementSyntaxContext<D> {
    type Body = Statement<D>;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        StatementSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    fn block_scope(&self, scope: D::Scope) -> D::Scope {
        self.ast_builder.file.make_scope(scope)
    }

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
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
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
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
