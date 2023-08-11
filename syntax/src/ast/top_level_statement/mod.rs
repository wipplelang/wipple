definitions! {
    mod r#use;
}

use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, Format, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use std::{collections::HashSet, mem};
use wipple_util::Shared;

syntax_group! {
    #[allow(clippy::large_enum_variant)]
    pub type TopLevelStatement<TopLevelStatementSyntaxContext> {
        non_terminal: {
            Use,
        },
        terminal: {
            Queued,
        },
    }
}

#[derive(Debug, Clone)]
pub struct QueuedTopLevelStatement<D: Driver> {
    pub span: D::Span,
    pub attributes: Vec<parse::Attribute<D>>,
    pub expr: parse::Expr<D>,
}

impl<D: Driver> QueuedTopLevelStatement<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for QueuedTopLevelStatement<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("finish parsing the queued statements first")
    }
}

#[derive(Clone)]
pub struct TopLevelStatementSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for TopLevelStatementSyntaxContext<D> {
    type Body = TopLevelStatement<D>;
    type Statement = parse::Expr<D>;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        TopLevelStatementSyntaxContext {
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
        _span: D::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        unimplemented!()
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope_set: Shared<HashSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(QueuedTopLevelStatement {
            span: expr.span,
            attributes: mem::take(&mut self.statement_attributes.unwrap().lock().raw),
            expr,
        }
        .into())
    }
}
