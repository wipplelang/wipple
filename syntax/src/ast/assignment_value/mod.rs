definitions! {
    mod expression;
    mod syntax;
    mod r#trait;
    mod r#type;
    mod type_function;
}

use crate::ScopeSet;
use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes, StatementSyntax,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
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
    assigned_name: Option<AssignedName<D>>,
}

#[derive(Clone)]
struct AssignedName<D: Driver> {
    name: D::InternedString,
    scope_set: ScopeSet<D::Scope>,
    did_create_syntax: Shared<bool>,
}

impl<D: Driver> AssignmentValueSyntaxContext<D> {
    pub(super) fn with_assigned_name(
        mut self,
        name: D::InternedString,
        scope_set: Shared<ScopeSet<D::Scope>>,
        did_create_syntax: Shared<bool>,
    ) -> Self {
        self.assigned_name = Some(AssignedName {
            name,
            scope_set: scope_set.lock().clone(),
            did_create_syntax,
        });

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
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_block(span, statements, scope_set)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        let expr = parse::Expr::list(expr.span, vec![expr]);

        context
            .build_terminal(expr, scope_set)
            .await
            .map(|expression| ExpressionAssignmentValue { expression }.into())
    }

    fn wrap_attributes(
        self,
        attributes: Result<Vec<parse::Attribute<D>>, parse::UnexpectedAttributeError<D>>,
        body: Self::Body,
    ) -> Self::Body {
        self.ast_builder.forbid_attributes(attributes);

        body
    }
}
