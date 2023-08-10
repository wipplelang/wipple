definitions! {
    mod allow_overlapping_instances;
    mod contextual;
    mod convert_from;
    mod derive;
    mod diagnostic_alias;
    mod diagnostic_item;
    mod help;
    mod help_group;
    mod help_playground;
    mod help_template;
    mod keyword;
    mod language_item;
    mod on_mismatch;
    mod on_unimplemented;
    mod operator_precedence;
    mod specialize;
}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type StatementAttribute<StatementAttributeSyntaxContext> {
        non_terminal: {
            AllowOverlappingInstances,
            Contextual,
            ConvertFrom,
            Derive,
            DiagnosticAlias,
            DiagnosticItem,
            Help,
            HelpGroup,
            HelpPlayground,
            HelpTemplate,
            Keyword,
            LanguageItem,
            OnMismatch,
            OnUnimplemented,
            OperatorPrecedence,
            Specialize,
        },
        terminal: {
            Unknown,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnknownStatementAttribute<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnknownStatementAttribute<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnknownStatementAttribute<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        unimplemented!("call `StatementAttributes::format` instead")
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

    fn block_scope(&self, scope: D::Scope) -> D::Scope {
        scope
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

        Ok(StatementAttribute::Unknown(UnknownStatementAttribute {
            span,
        }))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(expr.span, "expected attribute");

        Ok(StatementAttribute::Unknown(UnknownStatementAttribute {
            span: expr.span,
        }))
    }
}
