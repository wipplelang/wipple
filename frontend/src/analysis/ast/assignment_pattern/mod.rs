mod instance;
mod pattern;
mod type_function;

pub use instance::InstanceAssignmentPattern;
pub use pattern::PatternAssignmentPattern;
pub use type_function::TypeFunctionAssignmentPattern;

use instance::*;
use pattern::*;
use type_function::*;

use crate::{
    analysis::ast::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, DestructuringSyntax, PatternSyntaxContext, StatementAttributes,
    },
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type AssignmentPattern<AssignmentPatternSyntaxContext> {
        non_terminal: {
            Instance,
            TypeFunction,
            Pattern,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct AssignmentPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for AssignmentPatternSyntaxContext {
    type Body = AssignmentPattern;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentPatternSyntaxContext {
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
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_block(span, statements, scope)
            .await
            .map(|pattern| PatternAssignmentPattern { pattern }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_terminal(expr, scope)
            .await
            .map(|pattern| PatternAssignmentPattern { pattern }.into())
    }
}

impl FileBodySyntaxContext for AssignmentPatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
