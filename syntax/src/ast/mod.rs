#![allow(clippy::type_complexity)]

mod attributes;
mod format;
mod macros;
mod syntax;

definitions! {
    mod assignment_pattern;
    mod assignment_value;
    mod constant_type_annotation;
    mod default_type_parameter;
    mod destructuring;
    mod expression;
    mod file_attribute;
    mod pattern;
    mod statement;
    mod statement_attribute;
    mod syntax_body;
    mod syntax_pattern;
    mod syntax_rule;
    mod top_level_statement;
    mod r#type;
    mod type_body;
    mod type_member;
    mod type_pattern;
    mod when_arm;
    mod when_body;
    mod when_pattern;
    mod with_clause;
}

pub use attributes::*;
pub use format::Format;
pub use syntax::SyntaxError;

use crate::ScopeSet;
use crate::{ast::macros::definitions, parse, Driver, DriverExt, File as _, Span};
use futures::{future::BoxFuture, stream, StreamExt};
use sync_wrapper::SyncFuture;
use syntax::{Syntax, SyntaxContext};
use wipple_util::Shared;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltinSyntaxDefinition {
    pub name: &'static str,
    pub operator: bool,
    pub help: &'static str,
    pub playground: &'static str,
    pub template: &'static str,
}

impl BuiltinSyntaxDefinition {
    pub(crate) const fn function(
        name: &'static str,
        help: &'static str,
        playground: &'static str,
        template: &'static str,
    ) -> Self {
        BuiltinSyntaxDefinition {
            name,
            operator: false,
            help,
            playground,
            template,
        }
    }

    pub(crate) const fn operator(
        name: &'static str,
        help: &'static str,
        playground: &'static str,
        template: &'static str,
    ) -> Self {
        BuiltinSyntaxDefinition {
            name,
            operator: true,
            help,
            playground,
            template,
        }
    }
}

impl BuiltinSyntaxDefinition {
    pub(crate) const INSTANCE: Self = Self::function(
        "instance",
        "Define a trait's value for a specific type or types.",
        "https://wipple.dev/std/examples/trait.txt",
        "instance {%trait%}",
    );

    pub(crate) const TYPE_FUNCTION: Self = Self::operator(
        "=>",
        "Define a type function.",
        "https://wipple.dev/std/examples/type-function.txt",
        "{%parameters%} => {%type%}",
    );

    pub(crate) const SYNTAX: Self = Self::function(
        "syntax",
        "Define custom syntax.",
        "https://wipple.dev/std/examples/syntax.txt",
        "syntax { {%rule%} -> {%expression%} }",
    );

    pub(crate) const TRAIT: Self = Self::function(
        "trait",
        "Define a trait.",
        "https://wipple.dev/std/examples/trait.txt",
        "trait {%type%}",
    );

    pub(crate) const TYPE: Self = Self::function(
        "type",
        "Define a type.",
        "https://wipple.dev/std/examples/type.txt",
        "type { {%fields%} }",
    );

    pub(crate) const ASSIGN: Self = Self::operator(
        ":",
        "Assign a value to a name.",
        "https://wipple.dev/std/examples/assign.txt",
        "{%name%} : {%value%}",
    );

    pub(crate) const ANNOTATE: Self = Self::operator(
        "::",
        "Annotate a value with a type.",
        "https://wipple.dev/std/examples/annotate.txt",
        "{%value%} :: {%type%}",
    );

    pub(crate) const END: Self = Self::function(
        "end",
        "Return from a function early with a value.",
        "https://wipple.dev/std/examples/end.txt",
        "end {%value%}",
    );

    pub(crate) const EXTERNAL: Self = Self::function(
        "external",
        "Call a function defined in a different programming language.",
        "https://wipple.dev/std/examples/external.txt",
        "external {%namespace%} {%identifier%} {%parameters%}",
    );

    pub(crate) const FUNCTION: Self = Self::operator(
        "->",
        "Define a function.",
        "https://wipple.dev/std/examples/function.txt",
        "{%input%} -> {%output%}",
    );

    pub(crate) const INFER: Self = Self::function(
        "infer",
        "Mark a type parameter as inferred.",
        "https://wipple.dev/std/examples/infer.txt",
        "infer {%type%}",
    );

    pub(crate) const SEMICOLON: Self = Self::operator(
        ";",
        "Create a tuple.",
        "https://wipple.dev/std/examples/semicolon.txt",
        "{%left%} ; {%right%}",
    );

    pub(crate) const WHEN: Self = Self::function(
        "when",
        "Make a choice by matching patterns.",
        "https://wipple.dev/std/examples/when.txt",
        "when {value} { {%pattern%} -> {%value%} }",
    );

    pub(crate) const WITH: Self = Self::function(
        "with",
        "Override the value of a contextual constant.",
        "https://wipple.dev/std/examples/with.txt",
        "with ({%constant%} : {%value%}) { {%code%} }",
    );

    pub(crate) const OR: Self = Self::operator(
        "or",
        "Match multiple patterns in succession, or compare two `Boolean` values.",
        "https://wipple.dev/std/examples/or.txt",
        "{%left%} or {%right%}",
    );

    pub(crate) const WHERE: Self = Self::operator(
        "where",
        "Add bounds to a pattern, or update the fields of a structure.",
        "https://wipple.dev/std/examples/where.txt",
        "{%parameters%} where {%bounds%}",
    );

    pub(crate) const USE: Self = Self::function(
        "use",
        "Include code from another file.",
        "https://wipple.dev/std/examples/use.txt",
        "use {%file%}",
    );
}

#[derive(Clone)]
pub struct File<D: Driver> {
    pub span: D::Span,
    pub attributes: FileAttributes<D>,
    pub root_scope: ScopeSet<D::Scope>,
    pub statements: Vec<Result<Statement<D>, SyntaxError<D>>>,
    pub file: D::File,
}

impl<D: Driver> Format<D> for File<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{}\n{}",
            self.attributes.format()?,
            self.statements
                .into_iter()
                .map(|statement| statement?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join("\n")
        ))
    }
}

impl<D: Driver> std::fmt::Debug for File<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Make `Format::format` take `self` by reference
        match self.clone().format() {
            Ok(code) => write!(f, "{}", code)?,
            Err(_) => write!(f, "<syntax error>")?,
        }

        writeln!(f)?;

        f.debug_struct("File")
            .field("span", &self.span)
            .field("attributes", &self.attributes)
            .field("statements", &self.statements)
            .field("file", &self.file)
            .finish()
    }
}

pub(crate) async fn build<D: Driver>(
    driver: D,
    path: D::InternedString,
    driver_file: D::File,
    parse_file: parse::File<D>,
    options: parse::Options,
) -> File<D> {
    let mut root_scope = ScopeSet::new();
    root_scope.insert(driver_file.make_scope());

    let scope_set = Shared::new(root_scope.clone());

    let ast_builder = AstBuilder {
        driver,
        file: driver_file,
        path: path.clone(),
        attributes: Default::default(),
        options,
    };

    for attribute in parse_file.attributes {
        let context = FileAttributeSyntaxContext::new(ast_builder.clone());

        if ast_builder
            .build_list::<FileAttributeSyntax>(
                context.clone(),
                attribute.span,
                attribute.exprs.clone(),
                scope_set.clone(),
            )
            .await
            .is_none()
        {
            // The result of a file attribute doesn't affect whether the
            // file can be parsed
            let _ = context
                .build_terminal(
                    parse::Expr::list_or_expr(attribute.span, attribute.exprs),
                    scope_set.clone(),
                )
                .await;
        }
    }

    let mut queue = Vec::new();

    let implicit_imports = ast_builder
        .attributes
        .lock()
        .no_implicit_use
        .is_none()
        .then(|| {
            if ast_builder.file.is_entrypoint() {
                ast_builder.driver.implicit_entrypoint_imports()
            } else {
                ast_builder.driver.implicit_dependency_imports()
            }
        });

    if let Some(implicit_imports) = implicit_imports.as_ref() {
        queue.extend(implicit_imports.clone());
    }

    let statements = stream::iter(parse_file.statements)
        .then(|statement| {
            ast_builder.build_statement::<TopLevelStatementSyntax>(
                TopLevelStatementSyntaxContext::new(ast_builder.clone()),
                statement,
                scope_set.clone(),
            )
        })
        .collect::<Vec<_>>()
        .await
        .into_iter()
        .flatten()
        .map(|result| result.unwrap_or_else(|_| unreachable!()))
        .map(|statement| match statement {
            TopLevelStatement::Use(statement) => {
                queue.push(statement.path);

                statement.statement
            }
            TopLevelStatement::Queued(statement) => statement,
        })
        .collect::<Vec<_>>();

    ast_builder
        .driver
        .queue_files(Some(path.clone()), queue)
        .await;

    if let Some(implicit_imports) = implicit_imports {
        for implicit_path in implicit_imports {
            ast_builder
                .driver
                .clone()
                .syntax_of(
                    Some((path.clone(), ast_builder.file.clone())),
                    None,
                    implicit_path,
                    options,
                )
                .await;
        }
    }

    let statements = stream::iter(statements)
        .then(|statement| {
            let mut line = parse::ListLine::from(vec![statement.expr]);
            line.attributes = statement.attributes;

            ast_builder.build_statement::<StatementSyntax>(
                StatementSyntaxContext::new(ast_builder.clone()),
                parse::Statement { lines: vec![line] },
                scope_set.clone(),
            )
        })
        .collect::<Vec<_>>()
        .await
        .into_iter()
        .flatten()
        .collect();

    File {
        span: parse_file.span,
        attributes: ast_builder.attributes.into_unique(),
        root_scope,
        statements,
        file: ast_builder.file,
    }
}

#[derive(Clone)]
pub(crate) struct AstBuilder<D: Driver> {
    driver: D,
    path: D::InternedString,
    file: D::File,
    attributes: Shared<FileAttributes<D>>,
    options: parse::Options,
}

impl<D: Driver> AstBuilder<D> {
    fn build_expr<S: Syntax<D>>(
        &self,
        context: S::Context,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> SyncFuture<BoxFuture<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>> {
        SyncFuture::new(Box::pin(async move {
            match expr.kind {
                parse::ExprKind::Block(statements) if !context.block_is_terminal() => {
                    let mut scope_set = scope_set.clone();

                    if S::Context::BLOCK_CREATES_SCOPE {
                        let mut new_scope_set = scope_set.lock().clone();
                        new_scope_set.insert(self.file.make_scope());
                        scope_set = Shared::new(new_scope_set);
                    }

                    let statements = stream::iter(statements)
                        .then(|statement| {
                            self.build_statement::<<S::Context as SyntaxContext<D>>::Statement>(
                                <<S::Context as SyntaxContext<D>>::Statement as Syntax<D>>::Context::new(self.clone()),
                                statement,
                                scope_set.clone(),
                            )
                        })
                        .collect::<Vec<_>>()
                        .await
                        .into_iter()
                        .flatten();

                    context.build_block(expr.span, statements, scope_set).await
                }
                parse::ExprKind::List(lines) => {
                    let exprs = lines
                        .into_iter()
                        .flat_map(|line| line.exprs)
                        .collect::<Vec<_>>();

                    match self
                        .build_list::<S>(
                            context.clone(),
                            expr.span,
                            exprs.clone(),
                            scope_set.clone(),
                        )
                        .await
                    {
                        Some(result) => result,
                        None => {
                            let expr = if <S::Context as SyntaxContext<D>>::PREFERS_LISTS {
                                parse::Expr::list(expr.span, exprs)
                            } else {
                                parse::Expr::list_or_expr(expr.span, exprs)
                            };

                            context.build_terminal(expr, scope_set).await
                        }
                    }
                }
                parse::ExprKind::Name(_, _) => {
                    match self
                        .build_list::<S>(
                            context.clone(),
                            expr.span,
                            vec![expr.clone()],
                            scope_set.clone(),
                        )
                        .await
                    {
                        Some(result) => result,
                        None => context.build_terminal(expr, scope_set).await,
                    }
                }
                _ => context.build_terminal(expr, scope_set).await,
            }
        }))
    }

    fn build_statement<S: Syntax<D>>(
        &self,
        context: S::Context,
        statement: parse::Statement<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> SyncFuture<BoxFuture<Option<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>>>
    {
        SyncFuture::new(Box::pin(async move {
            let attributes = Shared::new(StatementAttributes::default());

            let mut attribute_exprs = Vec::new();
            let mut exprs = Vec::new();
            for line in statement.lines {
                attribute_exprs.extend(line.attributes);
                exprs.extend(line.exprs);
            }

            attributes.lock().raw = attribute_exprs.clone();

            let attributes_span = attribute_exprs
                .first()
                .map(|attribute| Span::join(attribute.span, attribute_exprs.last().unwrap().span));

            for attribute in attribute_exprs {
                let context = StatementAttributeSyntaxContext::new(self.clone())
                    .with_statement_attributes(attributes.clone());

                if self
                    .build_list::<StatementAttributeSyntax>(
                        context.clone(),
                        attribute.span,
                        attribute.exprs.clone(),
                        scope_set.clone(),
                    )
                    .await
                    .is_none()
                {
                    // The result of a statement attribute doesn't affect whether the
                    // statement's expression can be parsed
                    let _ = context
                        .build_terminal(
                            parse::Expr::list_or_expr(attribute.span, attribute.exprs),
                            scope_set.clone(),
                        )
                        .await;
                }
            }

            if exprs.is_empty() {
                if let Some(span) = attributes_span {
                    self.driver
                        .syntax_error(span, "expected an expression after attribute");
                }

                return None;
            }

            let span = Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

            let context = context.with_statement_attributes(attributes.clone());

            let result = match self
                .build_list::<S>(context.clone(), span, exprs.clone(), scope_set.clone())
                .await
            {
                Some(result) => result,
                None => {
                    context
                        .build_terminal(parse::Expr::list_or_expr(span, exprs), scope_set)
                        .await
                }
            };

            Some(result)
        }))
    }
}
