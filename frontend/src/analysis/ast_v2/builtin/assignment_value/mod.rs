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

use r#trait::*;
use r#type::*;
use syntax::*;
use type_function::*;

use crate::{
    analysis::ast_v2::{
        builtin::syntax::{SyntaxContext, SyntaxError},
        AstBuilder,
    },
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
        },
        terminal: {
            Expression,
        },
    }
}

#[derive(Clone)]
pub struct AssignmentValueSyntaxContext {
    pub(super) ast_builder: AstBuilder,
}

#[async_trait]
impl SyntaxContext for AssignmentValueSyntaxContext {
    type Body = AssignmentValue;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentValueSyntaxContext { ast_builder }
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
