definitions! {
    mod expression;
    mod syntax;
    mod r#trait;
    mod r#type;
    mod type_function;
    mod snippet;
}

use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes, StatementSyntax,
    },
    parse, Driver,
};
use crate::{File, ScopeSet};
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
            Snippet,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct AssignmentValueSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
    assigned_name: Option<AssignedName<D>>,
    assigned_snippet: Option<AssignedSnippet<D>>,
}

#[derive(Clone)]
struct AssignedName<D: Driver> {
    name: D::InternedString,
    scope_set: ScopeSet<D::Scope>,
}

#[derive(Clone)]
struct AssignedSnippet<D: Driver> {
    name: D::InternedString,
}

impl<D: Driver> AssignmentValueSyntaxContext<D> {
    pub(super) fn with_assigned_name(
        mut self,
        name: D::InternedString,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Self {
        self.assigned_name = Some(AssignedName {
            name,
            scope_set: scope_set.lock().clone(),
        });

        self
    }

    pub(super) fn with_assigned_snippet(mut self, name: D::InternedString) -> Self {
        self.assigned_snippet = Some(AssignedSnippet { name });
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
            assigned_snippet: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    fn block_is_terminal(&self) -> bool {
        self.assigned_snippet.is_some()
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
        mut expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        if let Some(assigned_snippet) = self.assigned_snippet {
            let mut wrap = false;
            expr.traverse_mut(|expr| wrap |= matches!(expr.kind, parse::ExprKind::QuoteName(_)));

            let value = SnippetAssignmentValue {
                name: Some(assigned_snippet.name.clone()),
                expression: expr,
                wrap,
            };

            self.ast_builder
                .file
                .define_snippet(assigned_snippet.name, value.clone());

            Ok(value.into())
        } else {
            let context = ExpressionSyntaxContext::new(self.ast_builder)
                .with_statement_attributes(self.statement_attributes.unwrap());

            let expr = parse::Expr::list(expr.span, vec![expr]);

            context
                .build_terminal(expr, scope_set)
                .await
                .map(|expression| ExpressionAssignmentValue { expression }.into())
        }
    }
}
