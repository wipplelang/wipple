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
        builtin::syntax::{SyntaxContext, SyntaxError},
        AstBuilder,
    },
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
}

#[async_trait]
impl SyntaxContext for PatternSyntaxContext {
    type Body = Pattern;

    fn new(ast_builder: AstBuilder) -> Self {
        PatternSyntaxContext { ast_builder }
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
