#[macro_use]
mod macros;
mod attributes;
mod syntax;

mod assignment_pattern;
mod assignment_value;
mod destructuring;
mod expression;
mod file_attribute;
mod pattern;
mod statement;
mod statement_attribute;
mod r#type;
mod type_pattern;

pub use attributes::*;

pub use assignment_pattern::*;
pub use assignment_value::*;
pub use destructuring::*;
pub use expression::*;
pub use file_attribute::*;
pub use pattern::*;
pub use r#type::*;
pub use statement::*;
pub use statement_attribute::*;
pub use type_pattern::*;

use crate::{
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    Compiler, FilePath, ScopeId, TemplateId,
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
    pub root_scope: ScopeId,
    pub scopes: BTreeMap<ScopeId, (Option<Span>, Option<ScopeId>)>,
    pub statements: Vec<Statement>,
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
            scopes: Default::default(),
            load: Arc::new(load),
        };

        let root_scope = self.new_scope_id_in(builder.file);

        let statements = stream::iter(file.statements)
            .then(|statement| {
                let context = StatementSyntaxContext::new(builder.clone());
                builder.build_statement::<StatementSyntax>(context, statement)
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .filter_map(Result::ok)
            .collect();

        File {
            span: file.span,
            attributes: builder.attributes.into_unique(),
            syntax_declarations: BTreeMap::new(),
            root_scope,
            scopes: builder
                .scopes
                .into_unique()
                .into_iter()
                .map(|(id, scope)| (id, (scope.span, scope.parent)))
                .collect(),
            statements,
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    id: ScopeId,
    span: Option<Span>,
    parent: Option<ScopeId>,
    syntaxes: HashMap<InternedString, SyntaxAssignmentValue>,
}

#[derive(Clone)]
struct AstBuilder {
    file: FilePath,
    compiler: Compiler,
    dependencies: Shared<HashMap<FilePath, (Arc<File>, Option<HashMap<InternedString, Span>>)>>,
    attributes: Shared<FileAttributes>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    load: Arc<
        dyn Fn(Compiler, Span, FilePath) -> BoxFuture<'static, Option<Arc<File>>> + Send + Sync,
    >,
}

impl AstBuilder {
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

            // The result of a statement attribute doesn't affect whether the
            // statement's expression can be parsed
            let _ = self
                .build_list::<StatementAttributeSyntax>(
                    context.clone(),
                    attribute.span,
                    &attribute.exprs,
                )
                .await
                .unwrap_or_else(|| {
                    context.build_terminal(parse::Expr::list(attribute.span, attribute.exprs))
                });
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

        Some(
            self.build_list::<S>(context.clone(), span, &exprs)
                .await
                .unwrap_or_else(|| context.build_terminal(parse::Expr::list(span, exprs))),
        )
    }

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
                        let context = <<<S as Syntax>::Context as SyntaxContext>::Statement as Syntax>::Context::new(self.clone());
                        self.build_statement::<<S::Context as SyntaxContext>::Statement>(
                            context, statement,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .flatten()
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>();

                context.build_block(expr.span, statements.into_iter()).await
            }
            parse::ExprKind::List(lines) => {
                let exprs = lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .collect::<Vec<_>>();

                self.build_list::<S>(context.clone(), expr.span, &exprs)
                    .await
                    .unwrap_or_else(|| context.build_terminal(parse::Expr::list(expr.span, exprs)))
            }
            _ => context.build_terminal(expr),
        }
    }
}
