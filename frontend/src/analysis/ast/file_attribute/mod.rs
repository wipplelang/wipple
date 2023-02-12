mod no_std;
mod recursion_limit;

pub use no_std::NoStdFileAttribute;
pub use recursion_limit::RecursionLimitFileAttribute;

use no_std::*;
use recursion_limit::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    diagnostics::Note,
    parse, ScopeId,
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
    type Body = FileAttribute;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        FileAttributeSyntaxContext { ast_builder }
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(span, "expected attribute")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "expected attribute")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
