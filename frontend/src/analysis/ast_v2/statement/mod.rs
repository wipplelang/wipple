mod annotate;
mod assign;
mod expression;
mod instance;
mod r#use;

// pub use annotate::AnnotateStatement;
// pub use assign::AssignStatement;
pub use expression::ExpressionStatement;
// pub use instance::InstanceStatement;
// pub use r#use::UseStatement;

use expression::*;
// use annotate::*;
// use assign::*;
// use instance::*;
// use r#use::*;

use crate::{
    analysis::ast_v2::{
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, ExpressionSyntaxContext, StatementAttributes,
    },
    diagnostics::Note,
    helpers::Shared,
    parse,
};
use async_trait::async_trait;

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Statement<StatementSyntaxContext> {
        non_terminal: {
            // Annotate,
            // Assign,
            // Instance,
            // Use,
            Expression,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct StatementSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for StatementSyntaxContext {
    type Body = Statement;
    type Statement = StatementSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        StatementSyntaxContext {
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
        let context = ExpressionSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context.build_block(span, statements).await.map(From::from)
    }

    async fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError> {
        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![Note::primary(expr.span, "invalid statement")],
        );

        Err(self.ast_builder.syntax_error(expr.span))
    }
}

impl FileBodySyntaxContext for StatementSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}
