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
        AstBuilder, Destructuring, DestructuringSyntax, PatternSyntaxContext,
    },
    diagnostics::Note,
    helpers::Shared,
    parse::{self, Span},
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type AssignmentPattern<AssignmentPatternSyntaxContext> {
        non_terminal: {
            Instance,
            TypeFunction,
            Pattern,
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
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
    ) -> Result<Self::Body, SyntaxError> {
        let context = PatternSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context.build_block(span, statements).await.map(From::from)
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "invalid pattern")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}

impl FileBodySyntaxContext for AssignmentPatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
