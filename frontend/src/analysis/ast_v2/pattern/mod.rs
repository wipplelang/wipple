mod text;
mod tuple;
mod r#where;

pub use r#where::WherePattern;
pub use text::TextPattern;
pub use tuple::TuplePattern;

use r#where::*;
use tuple::*;

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
    pub type Pattern<PatternSyntaxContext> {
        non_terminal: {
            Where,
            Tuple,
        },
        terminal: {
            Text,
            // TODO
        },
    }
}

#[derive(Clone)]
pub struct PatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for PatternSyntaxContext {
    type Body = Pattern;
    type Statement = PatternSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        PatternSyntaxContext {
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

impl FileBodySyntaxContext for PatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
