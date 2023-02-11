mod r#where;

pub use r#where::{WhereTypePattern, WhereTypePatternBound};

use r#where::*;

use crate::{
    analysis::ast_v2::{
        syntax::{ErrorSyntax, FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder,
    },
    helpers::{InternedString, Shared},
    parse::{self, Span},
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type TypePattern<TypePatternSyntaxContext> {
        non_terminal: {
            Where,
        },
        terminal: {
            Name,
            List,
        },
    }
}

#[derive(Debug, Clone)]
pub struct NameTypePattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct ListTypePattern {
    pub span: Span,
    pub patterns: Vec<TypePattern>,
}

#[derive(Clone)]
pub struct TypePatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for TypePatternSyntaxContext {
    type Body = TypePattern;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        TypePatternSyntaxContext {
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
        todo!()
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}

impl FileBodySyntaxContext for TypePatternSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
