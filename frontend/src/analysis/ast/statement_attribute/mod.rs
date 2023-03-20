mod allow_overlapping_instances;
mod diagnostic_item;
mod help;
mod keyword;
mod language_item;
mod on_mismatch;
mod on_unimplemented;
mod operator_precedence;
mod specialize;

pub use allow_overlapping_instances::AllowOverlappingInstancesStatementAttribute;
pub use diagnostic_item::{DiagnosticItemStatementAttribute, DiagnosticItemStatementAttributeKind};
pub use help::HelpStatementAttribute;
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
use diagnostic_item::*;
use help::*;
use keyword::*;
use language_item::*;
use on_mismatch::*;
use on_unimplemented::*;
use operator_precedence::*;
use specialize::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    diagnostics::Note,
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type StatementAttribute<StatementAttributeSyntaxContext> {
        non_terminal: {
            AllowOverlappingInstances,
            DiagnosticItem,
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
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for StatementAttributeSyntaxContext {
    type Body = StatementAttribute;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        StatementAttributeSyntaxContext {
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
        span: parse::SpanList,
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
            vec![Note::primary(span, "expected attribute")],
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
            vec![Note::primary(expr.span, "expected attribute")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
