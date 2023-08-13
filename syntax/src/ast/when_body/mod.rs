definitions! {}

use crate::ScopeSet;
use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, WhenArm, WhenArmSyntax,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type WhenBody<WhenBodySyntaxContext> {
        non_terminal: {},
        terminal: {
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct BlockWhenBody<D: Driver> {
    pub span: D::Span,
    pub arms: Vec<Result<WhenArm<D>, SyntaxError<D>>>,
}

impl<D: Driver> BlockWhenBody<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for BlockWhenBody<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{{\n{}\n}}",
            self.arms
                .into_iter()
                .map(|arm| arm?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join("\n")
        ))
    }
}

#[derive(Clone)]
pub struct WhenBodySyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for WhenBodySyntaxContext<D> {
    type Body = WhenBody<D>;
    type Statement = WhenArmSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        WhenBodySyntaxContext {
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
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(BlockWhenBody {
            span,
            arms: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected a block");

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
