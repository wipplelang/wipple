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
    ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes, StatementSyntax,
    },
    parse, Driver, File,
};
use async_trait::async_trait;
use wipple_util::Shared;

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
pub struct AssignmentValueSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
    assigned_name: Option<(D::InternedString, D::Span, D::Scope, Shared<bool>)>,
}

impl<D: Driver> AssignmentValueSyntaxContext<D> {
    pub(super) fn with_assigned_name(
        mut self,
        name: D::InternedString,
        span: D::Span,
        scope: D::Scope,
        did_create_syntax: Shared<bool>,
    ) -> Self {
        self.assigned_name = Some((name, span, scope, did_create_syntax));
        self
    }
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for AssignmentValueSyntaxContext<D> {
    type Body = AssignmentValue<D>;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        AssignmentValueSyntaxContext {
            ast_builder,
            statement_attributes: None,
            assigned_name: None,
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
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_block(span, statements, scope)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        let expr = parse::Expr::list(expr.span, vec![expr]);

        context
            .build_terminal(expr, scope)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }
}
