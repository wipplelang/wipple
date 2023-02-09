mod allow_overlapping_instances;
mod help;
mod keyword;
mod language_item;
mod on_mismatch;
mod on_unimplemented;
mod operator_precedence;
mod specialize;

pub use allow_overlapping_instances::AllowOverlappingInstancesStatementAttribute;
pub use help::HelpStatementAttribute;
pub use keyword::KeywordStatementAttribute;
pub use language_item::LanguageItemStatementAttribute;
pub use on_mismatch::OnMismatchStatementAttribute;
pub use on_unimplemented::OnUnimplementedStatementAttribute;
pub use operator_precedence::OperatorPrecedenceStatementAttribute;
pub use specialize::SpecializeStatementAttribute;

use allow_overlapping_instances::*;
use help::*;
use keyword::*;
use language_item::*;
use on_mismatch::*;
use on_unimplemented::*;
use operator_precedence::*;
use specialize::*;

use crate::{
    analysis::ast_v2::{
        builtin::syntax::{FileBodySyntaxContext, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    helpers::Shared,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type StatementAttribute<StatementAttributeSyntaxContext> {
        non_terminal: {
            AllowOverlappingInstances,
            Help,
            Keyword,
            LanguageItem,
            OnMismatch,
            OnUnimplemented,
            OperatorPrecedence,
            Specialize,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct StatementAttributeSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for StatementAttributeSyntaxContext {
    type Body = ();

    fn new(ast_builder: AstBuilder) -> Self {
        StatementAttributeSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl IntoIterator<Item = parse::Statement> + Send,
    ) -> Result<Self::Body, SyntaxError> {
        todo!()
    }

    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}

impl FileBodySyntaxContext for StatementAttributeSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
