mod end;
mod external;
mod format;
mod function;
mod tuple;
mod when;

// pub use end::EndExpression;
// pub use external::ExternalExpression;
// pub use format::FormatExpression;
// pub use function::FunctionExpression;
// pub use tuple::TupleExpression;
// pub use when::WhenExpression;

// use end::*;
// use external::*;
// use format::*;
// use function::*;
// use tuple::*;
// use when::*;

use crate::{
    analysis::ast_v2::{
        builtin::syntax::{SyntaxContext, SyntaxError},
        AstBuilder,
    },
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Expression<ExpressionSyntaxContext> {
        non_terminal: {
            // End,
            // External,
            // Format,
            // Function,
            // Tuple,
            // When,
        },
        terminal: {
            // Name,
            // Text,
            // etc.
        },
    }
}

#[derive(Clone)]
pub struct ExpressionSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    scope: Option<ScopeId>,
}

#[async_trait]
impl SyntaxContext for ExpressionSyntaxContext {
    type Body = Expression;

    fn new(ast_builder: AstBuilder) -> Self {
        ExpressionSyntaxContext {
            ast_builder,
            scope: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl IntoIterator<Item = parse::Statement> + Send,
    ) -> Result<Self::Body, SyntaxError> {
        todo!()
    }

    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}

impl ExpressionSyntaxContext {
    fn with_scope(mut self, scope: ScopeId) -> Self {
        self.scope = Some(scope);
        self
    }
}
