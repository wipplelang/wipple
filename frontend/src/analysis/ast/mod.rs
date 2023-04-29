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
mod syntax_body;
mod syntax_pattern;
mod syntax_rule;
mod r#type;
mod type_body;
mod type_member;
mod type_pattern;
mod when_arm;
mod when_body;
mod when_pattern;
mod with_clause;

pub use attributes::*;
pub use syntax::SyntaxError;

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
pub use syntax_body::*;
pub use syntax_pattern::*;
pub use syntax_rule::*;
pub use type_body::*;
pub use type_member::*;
pub use type_pattern::*;
pub use when_arm::*;
pub use when_body::*;
pub use when_pattern::*;
pub use with_clause::*;

use crate::{
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span, SpanList},
    Compiler, FilePath, ScopeId, SyntaxId,
};
use futures::{future::BoxFuture, stream, StreamExt};
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};
use syntax::{Syntax, SyntaxContext};

#[derive(Debug, Clone)]
pub struct File {
    pub span: Span,
    pub dependencies: Vec<Arc<File>>,
    pub attributes: FileAttributes,
    pub syntax_declarations: BTreeMap<SyntaxId, SyntaxAssignmentValue>,
    pub statements: Vec<Result<Statement, SyntaxError>>,
    pub exported: HashMap<InternedString, SyntaxId>,
    pub root_scope: ScopeId,
    scopes: BTreeMap<ScopeId, Scope>,
}

#[derive(Debug, Clone)]
pub struct Options {
    pub expand_syntax: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            expand_syntax: true,
        }
    }
}

impl Compiler {
    pub(crate) async fn build_ast(
        &self,
        file: parse::File,
        options: Options,
        load: impl Fn(Compiler, SpanList, FilePath) -> BoxFuture<'static, Option<Arc<File>>>
            + 'static
            + Send
            + Sync,
    ) -> File {
        let mut ast_builder = AstBuilder {
            options,
            file: file.span.path,
            compiler: self.clone(),
            dependencies: Default::default(),
            attributes: Default::default(),
            syntax_declarations: Default::default(),
            scopes: Default::default(),
            load: Arc::new(load),
            file_scope: None,
        };

        let scope = ast_builder.root_scope();
        ast_builder.file_scope = Some(scope);

        for attribute in file.attributes {
            let context = FileAttributeSyntaxContext::new(ast_builder.clone());

            if ast_builder
                .build_list::<FileAttributeSyntax>(
                    context.clone(),
                    attribute.span.into(),
                    attribute.exprs.clone(),
                    scope,
                )
                .await
                .is_none()
            {
                // The result of a file attribute doesn't affect whether the
                // file can be parsed
                let _ = context
                    .build_terminal(
                        parse::Expr::list_or_expr(attribute.span, attribute.exprs),
                        scope,
                    )
                    .await;
            }
        }

        if ast_builder.attributes.lock().no_std.is_none() {
            let std_path = ast_builder.compiler.loader.std_path();
            if let Some(std_path) = std_path {
                if let Some(file) =
                    (ast_builder.load)(ast_builder.compiler.clone(), file.span.into(), std_path)
                        .await
                {
                    ast_builder.add_dependency(file);
                }
            } else {
                ast_builder.compiler.add_error(
                    "standard library is missing, but this file requires it",
                    vec![Note::primary(
                        file.span.with_end(file.span.start),
                        "try adding `[[no-std]]` to this file to prevent automatically loading the standard library",
                    )],
                );
            }
        }

        let statements = stream::iter(file.statements)
            .then(|statement| {
                ast_builder.build_statement::<StatementSyntax>(
                    StatementSyntaxContext::new(ast_builder.clone()),
                    statement,
                    scope,
                )
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect();

        let exported = ast_builder
            .scopes
            .lock()
            .get(&scope)
            .unwrap()
            .syntaxes
            .clone()
            .into_iter()
            .filter_map(|(name, id)| Some((name, id?)))
            .collect();

        File {
            span: file.span,
            dependencies: ast_builder.dependencies.into_unique(),
            attributes: ast_builder.attributes.into_unique(),
            syntax_declarations: ast_builder.syntax_declarations.into_unique(),
            statements,
            scopes: ast_builder.scopes.into_unique(),
            exported,
            root_scope: scope,
        }
    }
}

#[derive(Clone)]
struct AstBuilder {
    options: Options,
    file: FilePath,
    compiler: Compiler,
    dependencies: Shared<Vec<Arc<File>>>,
    attributes: Shared<FileAttributes>,
    syntax_declarations: Shared<BTreeMap<SyntaxId, SyntaxAssignmentValue>>,
    scopes: Shared<BTreeMap<ScopeId, Scope>>,
    file_scope: Option<ScopeId>,
    load: Arc<
        dyn Fn(Compiler, SpanList, FilePath) -> BoxFuture<'static, Option<Arc<File>>> + Send + Sync,
    >,
}

#[derive(Debug, Clone, Default)]
struct Scope {
    parent: Option<ScopeId>,
    syntaxes: HashMap<InternedString, Option<SyntaxId>>,
}

impl AstBuilder {
    fn root_scope(&self) -> ScopeId {
        let id = self.compiler.new_scope_id_in(self.file);
        self.scopes.lock().insert(id, Scope::default());
        id
    }

    fn child_scope(&self, parent: ScopeId) -> ScopeId {
        let id = self.compiler.new_scope_id_in(self.file);

        self.scopes.lock().insert(
            id,
            Scope {
                parent: Some(parent),
                ..Default::default()
            },
        );

        id
    }

    fn add_syntax(&self, name: InternedString, syntax: SyntaxAssignmentValue, scope: ScopeId) {
        let id = self.compiler.new_syntax_id_in(self.file);

        self.syntax_declarations.lock().insert(id, syntax);

        self.scopes
            .lock()
            .get_mut(&scope)
            .unwrap()
            .syntaxes
            .insert(name, Some(id));
    }

    fn add_barrier(&self, name: InternedString, scope: ScopeId) {
        self.scopes
            .lock()
            .get_mut(&scope)
            .unwrap()
            .syntaxes
            .insert(name, None);
    }

    fn try_get_syntax(
        &self,
        name: InternedString,
        span: SpanList,
        scope: ScopeId,
    ) -> Option<SyntaxAssignmentValue> {
        let scopes = self.scopes.lock();

        let mut parent = Some(scope);
        while let Some(scope) = parent {
            let scope = scopes.get(&scope).unwrap();

            if let Some(syntax) = scope.syntaxes.get(&name) {
                let mut syntax_declarations = self.syntax_declarations.lock();
                let syntax = syntax_declarations.get_mut(syntax.as_ref()?).unwrap();
                syntax.uses.insert(span);

                return Some(syntax.clone());
            }

            parent = scope.parent;
        }

        None
    }
}

impl AstBuilder {
    async fn build_expr<S: Syntax>(
        &self,
        context: S::Context,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<<S::Context as SyntaxContext>::Body, SyntaxError> {
        match expr.kind {
            parse::ExprKind::Block(statements) => {
                let scope = context.block_scope(scope);

                let statements = stream::iter(statements)
                    .then(|statement| {
                        self.build_statement::<<S::Context as SyntaxContext>::Statement>(
                            <<S::Context as SyntaxContext>::Statement as Syntax>::Context::new(
                                self.clone(),
                            ),
                            statement,
                            scope,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await
                    .into_iter()
                    .flatten();

                context.build_block(expr.span, statements, scope).await
            }
            parse::ExprKind::List(lines) => {
                let exprs = lines
                    .into_iter()
                    .flat_map(|line| line.exprs)
                    .collect::<Vec<_>>();

                match self
                    .build_list::<S>(context.clone(), expr.span, exprs.clone(), scope)
                    .await
                {
                    Some(result) => result,
                    None => {
                        let expr = if <S::Context as SyntaxContext>::PREFERS_LISTS {
                            parse::Expr::list(expr.span, exprs)
                        } else {
                            parse::Expr::list_or_expr(expr.span, exprs)
                        };

                        context.build_terminal(expr, scope).await
                    }
                }
            }
            parse::ExprKind::Name(_, _) => {
                match self
                    .build_list::<S>(context.clone(), expr.span, vec![expr.clone()], scope)
                    .await
                {
                    Some(result) => result,
                    None => context.build_terminal(expr, scope).await,
                }
            }
            _ => context.build_terminal(expr, scope).await,
        }
    }

    async fn build_statement<S: Syntax>(
        &self,
        context: S::Context,
        statement: parse::Statement,
        scope: ScopeId,
    ) -> Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>> {
        let attributes = Shared::new(StatementAttributes::default());

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
                    attribute.span.into(),
                    attribute.exprs.clone(),
                    scope,
                )
                .await
                .is_none()
            {
                // The result of a statement attribute doesn't affect whether the
                // statement's expression can be parsed
                let _ = context
                    .build_terminal(
                        parse::Expr::list_or_expr(attribute.span, attribute.exprs),
                        scope,
                    )
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

        let span = parse::Span::join(
            exprs.first().unwrap().span.first(),
            exprs.last().unwrap().span.first(),
        );

        let context = context.with_statement_attributes(attributes.clone());

        let result = match self
            .build_list::<S>(context.clone(), span.into(), exprs.clone(), scope)
            .await
        {
            Some(result) => result,
            None => {
                context
                    .build_terminal(parse::Expr::list_or_expr(span, exprs), scope)
                    .await
            }
        };

        Some(result)
    }
}

impl AstBuilder {
    fn add_dependency(&self, file: Arc<File>) {
        self.syntax_declarations
            .lock()
            .extend(file.syntax_declarations.clone());

        self.scopes.lock().extend(file.scopes.clone());

        self.scopes
            .lock()
            .get_mut(self.file_scope.as_ref().unwrap())
            .unwrap()
            .syntaxes
            .extend(
                file.exported
                    .clone()
                    .into_iter()
                    .map(|(name, id)| (name, Some(id))),
            );

        self.dependencies.lock().push(file);
    }
}
