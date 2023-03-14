mod pattern;
mod r#where;

pub use pattern::PatternWhenPattern;
pub use r#where::WhereWhenPattern;

use pattern::*;
use r#where::*;

use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, DestructuringSyntax, PatternSyntaxContext, StatementAttributes,
    },
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type WhenPattern<WhenPatternSyntaxContext> {
        non_terminal: {
            Where,
            Pattern,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct WhenPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for WhenPatternSyntaxContext {
    type Body = WhenPattern;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        WhenPatternSyntaxContext {
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
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_block(span, statements, scope)
            .await
            .map(|pattern| PatternWhenPattern { pattern }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.as_ref().unwrap().clone());

        context
            .build_terminal(expr, scope)
            .await
            .map(|pattern| PatternWhenPattern { pattern }.into())
    }
}
