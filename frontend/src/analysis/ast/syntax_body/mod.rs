use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, SyntaxRule, SyntaxRuleSyntax,
    },
    diagnostics::Note,
    helpers::Shared,
    parse::{self, Span},
    ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type SyntaxBody<SyntaxBodySyntaxContext> {
        non_terminal: {},
        terminal: {
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct BlockSyntaxBody {
    pub span: Span,
    pub rules: Vec<Result<SyntaxRule, SyntaxError>>,
}

impl BlockSyntaxBody {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone)]
pub struct SyntaxBodySyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for SyntaxBodySyntaxContext {
    type Body = SyntaxBody;
    type Statement = SyntaxRuleSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        SyntaxBodySyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
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
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Ok(BlockSyntaxBody {
            span,
            rules: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "expected a block")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
