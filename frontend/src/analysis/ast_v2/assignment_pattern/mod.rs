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
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax,
    },
    helpers::Shared,
    parse::{self, Span},
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
            Destructure,
        },
    }
}

#[derive(Debug, Clone)]
pub struct DestructureAssignmentPattern {
    pub span: Span,
    pub destructurings: Vec<Destructuring>,
}

#[derive(Clone)]
pub struct AssignmentPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for AssignmentPatternSyntaxContext {
    type Body = AssignmentPattern;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        AssignmentPatternSyntaxContext {
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
        Ok(DestructureAssignmentPattern {
            span,
            destructurings: statements.into_iter().collect(),
        }
        .into())
    }

    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!();
    }
}

impl FileBodySyntaxContext for AssignmentPatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
