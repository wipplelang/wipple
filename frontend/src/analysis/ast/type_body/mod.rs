use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, TypeMember, TypeMemberSyntax,
    },
    diagnostics::Note,
    helpers::Shared,
    parse::{self, Span},
    ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type TypeBody<TypeBodySyntaxContext> {
        non_terminal: {},
        terminal: {
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct BlockTypeBody {
    pub span: Span,
    pub members: Vec<Result<TypeMember, SyntaxError>>,
}

#[derive(Clone)]
pub struct TypeBodySyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for TypeBodySyntaxContext {
    type Body = TypeBody;
    type Statement = TypeMemberSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        TypeBodySyntaxContext {
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
        Ok(BlockTypeBody {
            span,
            members: statements.collect(),
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
