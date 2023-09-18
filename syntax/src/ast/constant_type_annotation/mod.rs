definitions! {
    mod r#type;
    mod type_function;
}

use crate::ScopeSet;
use crate::{
    ast::{
        macros::{definitions, syntax_group},
        syntax::{ErrorSyntax, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes, TypeSyntaxContext,
    },
    parse, Driver,
};
use async_trait::async_trait;
use wipple_util::Shared;

syntax_group! {
    pub type ConstantTypeAnnotation<ConstantTypeAnnotationSyntaxContext> {
        non_terminal: {
            TypeFunction,
            Type,
        },
        terminal: {},
    }
}

#[derive(Clone)]
pub struct ConstantTypeAnnotationSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for ConstantTypeAnnotationSyntaxContext<D> {
    type Body = ConstantTypeAnnotation<D>;
    type Statement = ErrorSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        ConstantTypeAnnotationSyntaxContext {
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
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        self.ast_builder
            .driver
            .syntax_error(span, "expected a type");

        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        let context = TypeSyntaxContext::new(self.ast_builder)
            .with_statement_attributes(self.statement_attributes.unwrap());

        context
            .build_terminal(expr, scope_set)
            .await
            .map(|ty| TypeConstantTypeAnnotation { ty }.into())
    }

    fn wrap_attributes(
        self,
        attributes: Result<Vec<parse::Attribute<D>>, parse::UnexpectedAttributeError<D>>,
        body: Self::Body,
    ) -> Self::Body {
        self.ast_builder.forbid_attributes(attributes);

        body
    }
}
