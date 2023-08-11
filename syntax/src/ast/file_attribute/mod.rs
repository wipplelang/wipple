definitions! {
    mod help_url;
    mod no_std;
    mod recursion_limit;
}

use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use std::collections::HashSet;
use wipple_util::Shared;

syntax_group! {
    pub type FileAttribute<FileAttributeSyntaxContext> {
        non_terminal: {
            HelpUrl,
            NoStd,
            RecursionLimit,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct FileAttributeSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for FileAttributeSyntaxContext<D> {
    type Body = FileAttribute<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        FileAttributeSyntaxContext { ast_builder }
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes<D>>) -> Self {
        self
    }

    async fn build_block(
        self,
        span: D::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(span, "expected attribute");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected attribute");

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
