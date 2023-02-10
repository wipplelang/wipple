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
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    helpers::Shared,
    parse,
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
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for ExpressionSyntaxContext {
    type Body = Expression;
    type Statement = ExpressionSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        ExpressionSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl Iterator<Item = <<Self::Statement as Syntax>::Context as SyntaxContext>::Body>
            + Send,
    ) -> Result<Self::Body, SyntaxError> {
        todo!()
    }

    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}

impl FileBodySyntaxContext for ExpressionSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
