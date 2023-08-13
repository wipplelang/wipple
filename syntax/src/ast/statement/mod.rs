definitions! {
    mod annotate;
    mod assign;
    mod expression;
    mod instance;
    mod type_function;
    mod r#use;
}

use crate::ScopeSet;
use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes,
    },
    parse, Driver,
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

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_block(span, statements, scope_set)
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
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context.build_terminal(expr, scope_set).await.map(|expr| {
            ExpressionStatement {
                expression: expr,
                attributes: self.statement_attributes.unwrap().lock().clone(),
            }
            .into()
        })
    }
}
