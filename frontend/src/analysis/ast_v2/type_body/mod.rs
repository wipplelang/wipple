use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, TypeMember, TypeMemberSyntax,
    },
    diagnostics::Note,
    helpers::Shared,
    parse::{self, Span},
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
    pub arms: Vec<Result<TypeMember, SyntaxError>>,
}

#[derive(Clone)]
pub struct TypeBodySyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
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
        Ok(BlockTypeBody {
            span,
            arms: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "expected a block")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}

impl FileBodySyntaxContext for TypeBodySyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
