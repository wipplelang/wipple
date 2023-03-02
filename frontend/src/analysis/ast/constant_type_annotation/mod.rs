mod r#type;
mod type_function;

pub use r#type::TypeConstantTypeAnnotation;
pub use type_function::TypeFunctionConstantTypeAnnotation;

use r#type::*;
use type_function::*;

use crate::{
    analysis::ast::{
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, TypeSyntaxContext,
    },
    diagnostics::Note,
    helpers::Shared,
    parse, ScopeId,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type ConstantTypeAnnotation<ConstantTypeAnnotationSyntaxContext> {
        non_terminal: {
            TypeFunction,
            Type,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct ConstantTypeAnnotationSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for ConstantTypeAnnotationSyntaxContext {
    type Body = ConstantTypeAnnotation;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        ConstantTypeAnnotationSyntaxContext {
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
        self.ast_builder
            .compiler
            .add_error("syntax error", vec![Note::primary(span, "expected a type")]);

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        let context = TypeSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_terminal(expr, scope)
            .await
            .map(|ty| TypeConstantTypeAnnotation { ty }.into())
    }
}
