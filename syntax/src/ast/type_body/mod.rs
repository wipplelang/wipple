definitions! {}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, TypeMember, TypeMemberSyntax,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type TypeBody<TypeBodySyntaxContext> {
        non_terminal: {},
        terminal: {
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct BlockTypeBody<D: Driver> {
    pub span: D::Span,
    pub members: Vec<Result<TypeMember<D>, SyntaxError<D>>>,
}

impl<D: Driver> BlockTypeBody<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for BlockTypeBody<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{{\n{}\n}}",
            self.members
                .into_iter()
                .map(|member| member?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join("\n")
        ))
    }
}

#[derive(Clone)]
pub struct TypeBodySyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for TypeBodySyntaxContext<D> {
    type Body = TypeBody<D>;
    type Statement = TypeMemberSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        TypeBodySyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(BlockTypeBody {
            span,
            members: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected a block");

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
