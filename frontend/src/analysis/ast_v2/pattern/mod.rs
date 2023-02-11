mod annotate;
mod tuple;
mod r#where;

pub use annotate::AnnotatePattern;
pub use r#where::WherePattern;
pub use tuple::TuplePattern;

use r#where::*;
use tuple::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Destructuring, DestructuringSyntax,
    },
    helpers::{InternedString, Shared},
    parse::{self, Span},
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Pattern<PatternSyntaxContext> {
        non_terminal: {
            Where,
            Tuple,
        },
        terminal: {
            Text,
            Destructure,
            // TODO
        },
    }
}

#[derive(Debug, Clone)]
pub struct TextPattern {
    pub span: Span,
    pub value: InternedString,
}

#[derive(Debug, Clone)]
pub struct DestructurePattern {
    pub span: Span,
    pub destructurings: Vec<Result<Destructuring, SyntaxError>>,
}

#[derive(Clone)]
pub struct PatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for PatternSyntaxContext {
    type Body = Pattern;
    type Statement = DestructuringSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        PatternSyntaxContext {
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
        Ok(DestructurePattern {
            span,
            destructurings: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}

impl FileBodySyntaxContext for PatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
