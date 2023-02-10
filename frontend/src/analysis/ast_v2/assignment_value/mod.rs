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
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementSyntax,
    },
    helpers::Shared,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type AssignmentValue<AssignmentValueSyntaxContext> {
        non_terminal: {
            Syntax,
            Trait,
            Type,
            TypeFunction,
            Expression,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct AssignmentValueSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for AssignmentValueSyntaxContext {
    type Body = AssignmentValue;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentValueSyntaxContext {
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
impl FileBodySyntaxContext for AssignmentValueSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
