mod annotate;
mod assign;
mod expression;
mod instance;
mod type_function;
mod r#use;

// pub use annotate::AnnotateStatement;
// pub use assign::AssignStatement;
pub use expression::ExpressionStatement;
// pub use instance::InstanceStatement;
// pub use r#use::UseStatement;
// pub use type_function::TypeFunctionStatement;

// use annotate::*;
// use assign::*;
// use instance::*;
// use r#use::*;
// use type_function::*;

use crate::{
    analysis::ast_v2::{
        builtin::syntax::{FileBodySyntaxContext, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    helpers::Shared,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Statement<StatementSyntaxContext> {
        non_terminal: {
            // Annotate,
            // Assign,
            // Instance,
            // Use,
            // TypeFunction,
        },
        terminal: {
            Expression,
        },
    }
}

#[derive(Clone)]
pub struct StatementSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for StatementSyntaxContext {
    type Body = Statement;

    fn new(ast_builder: AstBuilder) -> Self {
        StatementSyntaxContext {
            ast_builder,
            statement_attributes: None,
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

impl FileBodySyntaxContext for StatementSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
