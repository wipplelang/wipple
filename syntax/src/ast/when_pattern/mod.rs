mod pattern;
mod r#where;

pub use pattern::PatternWhenPattern;
pub use r#where::WhereWhenPattern;

use pattern::*;
use r#where::*;

use crate::{
    ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, DestructuringSyntax, PatternSyntaxContext, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type WhenPattern<WhenPatternSyntaxContext> {
        non_terminal: {
            Where,
            Pattern,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct WhenPatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for WhenPatternSyntaxContext<D> {
    type Body = WhenPattern<D>;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        WhenPatternSyntaxContext {
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
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_block(span, statements, scope)
            .await
            .map(|pattern| PatternWhenPattern { pattern }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_terminal(expr, scope)
            .await
            .map(|pattern| PatternWhenPattern { pattern }.into())
    }
}
