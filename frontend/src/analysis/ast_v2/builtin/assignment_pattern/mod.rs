mod instance;
mod pattern;
mod type_function;

pub use instance::InstanceAssignmentPattern;
pub use pattern::PatternAssignmentPattern;
pub use type_function::TypeFunctionAssignmentPattern;

use instance::*;
use pattern::*;
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
    pub type AssignmentPattern<AssignmentPatternSyntaxContext> {
        non_terminal: {
            Instance,
            Pattern,
            TypeFunction,
        },
        terminal: {

        },
    }
}

#[derive(Clone)]
pub struct AssignmentPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
}

#[async_trait]
impl SyntaxContext for AssignmentPatternSyntaxContext {
    type Body = AssignmentPattern;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentPatternSyntaxContext { ast_builder }
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
