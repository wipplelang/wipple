mod no_std;
mod recursion_limit;

pub use no_std::NoStdFileAttribute;
pub use recursion_limit::RecursionLimitFileAttribute;

use no_std::*;
use recursion_limit::*;

use crate::{
    analysis::ast_v2::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type FileAttribute<FileAttributeSyntaxContext> {
        non_terminal: {
            NoStd,
            RecursionLimit,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct FileAttributeSyntaxContext {
    pub(super) ast_builder: AstBuilder,
}

#[async_trait]
impl SyntaxContext for FileAttributeSyntaxContext {
    type Body = ();
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        FileAttributeSyntaxContext { ast_builder }
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
