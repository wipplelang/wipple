mod expression;
mod syntax;
mod r#trait;
mod r#type;
mod type_function;

pub use expression::ExpressionAssignmentValue;
pub use r#trait::TraitAssignmentValue;
pub use r#type::TypeAssignmentValue;
pub use syntax::SyntaxAssignmentValue;
pub use type_function::TypeFunctionAssignmentValue;

use expression::*;
use r#trait::*;
use r#type::*;
use syntax::*;
use type_function::*;

use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes, StatementSyntax,
    },
    helpers::{InternedString, Shared},
    parse::{self, SpanList},
    ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type AssignmentValue<AssignmentValueSyntaxContext> {
        non_terminal: {
            Trait,
            Type,
            Syntax,
            TypeFunction,
            Expression,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct AssignmentValueSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
    assigned_name: Option<(InternedString, SpanList, ScopeId, Shared<bool>)>,
}

impl AssignmentValueSyntaxContext {
    pub(super) fn with_assigned_name(
        mut self,
        name: InternedString,
        span: SpanList,
        scope: ScopeId,
        did_create_syntax: Shared<bool>,
    ) -> Self {
        self.assigned_name = Some((name, span, scope, did_create_syntax));
        self
    }
}

#[async_trait]
impl SyntaxContext for AssignmentValueSyntaxContext {
    type Body = AssignmentValue;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentValueSyntaxContext {
            ast_builder,
            statement_attributes: None,
            assigned_name: None,
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
        span: parse::SpanList,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_block(span, statements, scope)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        let expr = parse::Expr::list(expr.span, vec![expr]);

        context
            .build_terminal(expr, scope)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }
}
