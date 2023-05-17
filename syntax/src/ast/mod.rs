mod attributes;
mod format;
mod macros;
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
pub use format::Format;
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

use crate::{parse, Driver, DriverExt, File as _, Span};
use futures::{future::BoxFuture, stream, StreamExt};

use sync_wrapper::SyncFuture;
use syntax::{Syntax, SyntaxContext};
use wipple_util::Shared;

#[derive(Clone)]
pub struct File<D: Driver> {
    pub span: D::Span,
    pub attributes: FileAttributes<D>,
    pub statements: Vec<Result<Statement<D>, SyntaxError<D>>>,
    pub file: D::File,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for File<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(File {
            span: Default::default(),
            attributes: Default::default(),
            statements: arbitrary::Arbitrary::arbitrary(u)?,
            file: crate::SingleFile(String::new()),
        })
    }
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
    path: D::Path,
    driver_file: D::File,
    parse_file: parse::File<D>,
) -> File<D> {
    let scope = driver_file.root_scope();

    let ast_builder = AstBuilder {
        driver,
        file: driver_file,
        path,
        attributes: Default::default(),
    };

    for attribute in parse_file.attributes {
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
        if let Some(std_path) = ast_builder.driver.std_path() {
            ast_builder
                .driver
                .clone()
                .syntax_of(Some((path, ast_builder.file.clone())), None, std_path)
                .await;
        }
    }

    let statements = stream::iter(parse_file.statements)
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

    File {
        span: parse_file.span,
        attributes: ast_builder.attributes.into_unique(),
        statements,
        file: ast_builder.file,
    }
}

#[derive(Clone)]
pub(crate) struct AstBuilder<D: Driver> {
    driver: D,
    path: D::Path,
    file: D::File,
    attributes: Shared<FileAttributes<D>>,
}

impl<D: Driver> AstBuilder<D> {
    fn build_expr<S: Syntax<D>>(
        &self,
        context: S::Context,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> SyncFuture<BoxFuture<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>> {
        SyncFuture::new(Box::pin(async move {
            match expr.kind {
                parse::ExprKind::Block(statements) => {
                    let scope = context.block_scope(scope);

                    let statements = stream::iter(statements)
                        .then(|statement| {
                            self.build_statement::<<S::Context as SyntaxContext<D>>::Statement>(
                                <<S::Context as SyntaxContext<D>>::Statement as Syntax<D>>::Context::new(self.clone()),
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
                            let expr = if <S::Context as SyntaxContext<D>>::PREFERS_LISTS {
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
        }))
    }

    fn build_statement<S: Syntax<D>>(
        &self,
        context: S::Context,
        statement: parse::Statement<D>,
        scope: D::Scope,
    ) -> SyncFuture<BoxFuture<Option<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>>>
    {
        SyncFuture::new(Box::pin(async move {
            let attributes = Shared::new(StatementAttributes::default());

            let (attribute_exprs, exprs) = (statement.line.attributes, statement.line.exprs);

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
        }))
    }
}
