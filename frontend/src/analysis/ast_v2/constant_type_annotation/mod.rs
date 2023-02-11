mod r#type;
mod type_function;

pub use r#type::TypeConstantTypeAnnotation;
pub use type_function::TypeFunctionConstantTypeAnnotation;

use r#type::*;
use type_function::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementSyntax,
    },
    diagnostics::Note,
    helpers::Shared,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type ConstantTypeAnnotation<ConstantTypeAnnotationSyntaxContext> {
        non_terminal: {
            Type,
            TypeFunction,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct ConstantTypeAnnotationSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<Vec<()> /* TODO */>>,
}

#[async_trait]
impl SyntaxContext for ConstantTypeAnnotationSyntaxContext {
    type Body = ConstantTypeAnnotation;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        ConstantTypeAnnotationSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(
                span,
                "block is not valid inside type annotation",
            )],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "expected type in type annotation")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}
impl FileBodySyntaxContext for ConstantTypeAnnotationSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<Vec<()> /* TODO */>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
