#[macro_use]
mod macros;
mod attributes;
mod syntax;

mod assignment_pattern;
mod assignment_value;
mod constant_type_annotation;
mod destructuring;
mod expression;
mod file_attribute;
mod pattern;
mod statement;
mod statement_attribute;
mod r#type;
mod type_body;
mod type_member;
mod type_pattern;
mod when_arm;
mod when_body;

pub use attributes::*;

pub use assignment_pattern::*;
pub use assignment_value::*;
pub use constant_type_annotation::*;
pub use destructuring::*;
pub use expression::*;
pub use file_attribute::*;
pub use pattern::*;
pub use r#type::*;
pub use statement::*;
pub use statement_attribute::*;
pub use type_body::*;
pub use type_member::*;
pub use type_pattern::*;
pub use when_arm::*;
pub use when_body::*;

use crate::{
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, TemplateId,
};
use futures::{future::BoxFuture, stream, StreamExt};
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};
use syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError};

#[derive(Debug, Clone)]
pub struct File {
    pub span: Span,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<TemplateId, SyntaxAssignmentValue>,
    pub statements: Vec<Result<Statement, SyntaxError>>,
}

impl Compiler {
    pub(crate) async fn build_ast_v2(
        &self,
        file: parse::File,
        load: impl Fn(Compiler, Span, FilePath) -> BoxFuture<'static, Option<Arc<File>>>
            + 'static
            + Send
            + Sync,
    ) -> File {
        let builder = AstBuilder {
            file: file.span.path,
            compiler: self.clone(),
            dependencies: Default::default(),
            attributes: Default::default(),
            load: Arc::new(load),
        };

        let statements = stream::iter(file.statements)
            .then(|statement| {
                let context = StatementSyntaxContext::new(builder.clone());
                builder.build_statement::<StatementSyntax>(context, statement)
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect();

        File {
            span: file.span,
            attributes: builder.attributes.into_unique(),
            syntax_declarations: BTreeMap::new(),
            statements,
        }
    }
}

#[derive(Clone)]
struct AstBuilder {
    file: FilePath,
    compiler: Compiler,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    load: Arc<
        dyn Fn(Compiler, Span, FilePath) -> BoxFuture<'static, Option<Arc<File>>> + Send + Sync,
    >,
}

impl AstBuilder {
    async fn build_expr<S: Syntax>(
        &self,
        context: S::Context,
        expr: parse::Expr,
    ) -> Result<<S::Context as SyntaxContext>::Body, SyntaxError>
    where
        S::Context: FileBodySyntaxContext,
        <<<S as Syntax>::Context as SyntaxContext>::Statement as Syntax>::Context:
            FileBodySyntaxContext,
    {
        match expr.kind {
            parse::ExprKind::Block(statements) => {
                let statements = stream::iter(statements)
                    .then(|statement| {
                        self.build_statement::<<S::Context as SyntaxContext>::Statement>(
                            <<S::Context as SyntaxContext>::Statement as Syntax>::Context::new(
                                self.clone(),
                            ),
                            statement,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .flatten();

                context.build_block(expr.span, statements).await
            }
            parse::ExprKind::List(lines) => {
                let exprs = lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .collect::<Vec<_>>();

                match self
                    .build_list::<S>(context.clone(), expr.span, &exprs)
                    .await
                {
                    Some(result) => result,
                    None => {
                        context
                            .build_terminal(parse::Expr::list(expr.span, exprs))
                            .await
                    }
                }
            }
            _ => context.build_terminal(expr).await,
        }
    }

    async fn build_statement<S: Syntax>(
        &self,
        context: S::Context,
        statement: parse::Statement,
    ) -> Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>>
    where
        S::Context: FileBodySyntaxContext,
    {
        let attributes = Shared::new(Vec::new()); // TODO

        let (attribute_exprs, exprs): (Vec<_>, Vec<_>) = statement
            .lines
            .into_iter()
            .map(|line| (line.attributes, line.exprs))
            .unzip();

        let attribute_exprs = attribute_exprs.into_iter().flatten().collect::<Vec<_>>();
        let attributes_span = attribute_exprs.first().map(|attribute| {
            parse::Span::join(attribute.span, attribute_exprs.last().unwrap().span)
        });

        for attribute in attribute_exprs {
            let context = StatementAttributeSyntaxContext::new(self.clone())
                .with_statement_attributes(attributes.clone());

            if self
                .build_list::<StatementAttributeSyntax>(
                    context.clone(),
                    attribute.span,
                    &attribute.exprs,
                )
                .await
                .is_none()
            {
                // The result of a statement attribute doesn't affect whether the
                // statement's expression can be parsed
                let _ = context
                    .build_terminal(parse::Expr::list(attribute.span, attribute.exprs))
                    .await;
            }
        }

        let exprs = exprs.into_iter().flatten().collect::<Vec<_>>();

        if exprs.is_empty() {
            if let Some(span) = attributes_span {
                self.compiler.add_error(
                    "cannot use attributes on an empty statement",
                    vec![Note::primary(span, "expected an expression after these")],
                );
            }

            return None;
        }

        let span = parse::Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

        let context = context.with_statement_attributes(attributes.clone());

        let result = match self.build_list::<S>(context.clone(), span, &exprs).await {
            Some(result) => result,
            None => context.build_terminal(parse::Expr::list(span, exprs)).await,
        };

        Some(result)
    }
}
