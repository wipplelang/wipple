definitions! {
    mod instance;
    mod pattern;
    mod type_function;
}

use crate::ScopeSet;
use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, DestructuringSyntax, PatternSyntaxContext, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
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
pub struct AssignmentPatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for AssignmentPatternSyntaxContext<D> {
    type Body = AssignmentPattern<D>;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        AssignmentPatternSyntaxContext {
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
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_block(span, statements, scope_set)
            .await
            .map(|pattern| PatternAssignmentPattern { pattern }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_terminal(expr, scope_set)
            .await
            .map(|pattern| PatternAssignmentPattern { pattern }.into())
    }
}
