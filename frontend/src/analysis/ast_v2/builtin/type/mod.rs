mod function;
mod tuple;

pub use function::FunctionType;
pub use tuple::TupleType;

use function::*;
use tuple::*;

use crate::{
    analysis::ast_v2::{
        builtin::syntax::{SyntaxContext, SyntaxError},
        AstBuilder,
    },
    diagnostics::Note,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Type<TypeSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
        },
        terminal: {
            // TODO
        },
    }
}

#[derive(Clone)]
pub struct TypeSyntaxContext {
    pub(super) ast_builder: AstBuilder,
}

#[async_trait]
impl SyntaxContext for TypeSyntaxContext {
    type Body = Type;

    fn new(ast_builder: AstBuilder) -> Self {
        TypeSyntaxContext { ast_builder }
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl IntoIterator<Item = parse::Statement> + Send,
    ) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(span, "block is not allowed here")],
        );

        Err(self.ast_builder.syntax_error(span))
    }

    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        todo!()
    }
}
