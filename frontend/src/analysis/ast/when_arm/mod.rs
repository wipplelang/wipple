mod function;

pub use function::FunctionWhenArm;

use function::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    diagnostics::Note,
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type WhenArm<WhenArmSyntaxContext> {
        non_terminal: {
            Function,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct WhenArmSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for WhenArmSyntaxContext {
    type Body = WhenArm;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        WhenArmSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
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
            vec![Note::primary(span, "expected a function")],
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
            vec![Note::primary(expr.span, "expected a function")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}

impl FileBodySyntaxContext for WhenArmSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
