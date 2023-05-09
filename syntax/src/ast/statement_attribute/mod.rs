mod allow_overlapping_instances;
mod contextual;
mod diagnostic_alias;
mod diagnostic_item;
mod help;
mod help_group;
mod keyword;
mod language_item;
mod on_mismatch;
mod on_unimplemented;
mod operator_precedence;
mod specialize;

pub use allow_overlapping_instances::AllowOverlappingInstancesStatementAttribute;
pub use contextual::ContextualStatementAttribute;
pub use diagnostic_alias::DiagnosticAliasStatementAttribute;
pub use diagnostic_item::{DiagnosticItemStatementAttribute, DiagnosticItemStatementAttributeKind};
pub use help::HelpStatementAttribute;
pub use help_group::HelpGroupStatementAttribute;
pub use keyword::KeywordStatementAttribute;
pub use language_item::{LanguageItemStatementAttribute, LanguageItemStatementAttributeKind};
pub use on_mismatch::OnMismatchStatementAttribute;
pub use on_unimplemented::OnUnimplementedStatementAttribute;
pub use operator_precedence::{
    OperatorAssociativity, OperatorPrecedenceStatementAttribute,
    OperatorPrecedenceStatementAttributeKind,
};
pub use specialize::SpecializeStatementAttribute;

use allow_overlapping_instances::*;
use contextual::*;
use diagnostic_alias::*;
use diagnostic_item::*;
use help::*;
use help_group::*;
use keyword::*;
use language_item::*;
use on_mismatch::*;
use on_unimplemented::*;
use operator_precedence::*;
use specialize::*;

use crate::{
    ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type StatementAttribute<StatementAttributeSyntaxContext> {
        non_terminal: {
            AllowOverlappingInstances,
            Contextual,
            DiagnosticAlias,
            DiagnosticItem,
            Help,
            HelpGroup,
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
pub struct StatementAttributeSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for StatementAttributeSyntaxContext<D> {
    type Body = StatementAttribute<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        StatementAttributeSyntaxContext {
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
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(span, "expected attribute");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected attribute");

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
