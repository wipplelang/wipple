#[macro_use]
mod macros;
mod attributes;
mod syntax;

mod assignment_pattern;
mod assignment_value;
mod expression;
mod file_attribute;
mod pattern;
mod statement;
mod statement_attribute;
mod r#type;

pub use attributes::*;

pub use assignment_pattern::*;
pub use assignment_value::*;
pub use expression::*;
pub use file_attribute::*;
pub use pattern::*;
pub use r#type::*;
pub use statement::*;
pub use statement_attribute::*;

use crate::{
    analysis::ast_v2::{
        builtin::{
            statement::{StatementSyntax, StatementSyntaxContext},
            statement_attribute::{StatementAttributeSyntax, StatementAttributeSyntaxContext},
            syntax::FileBodySyntaxContext,
        },
        AstBuilder,
    },
    diagnostics::Note,
    helpers::Shared,
    parse,
};
use futures::{stream, StreamExt};
use syntax::{Syntax, SyntaxContext, SyntaxError};

pub struct StatementWithAttributes<S> {
    pub attributes: Vec<()>, // TODO
    pub statement: S,
}

impl AstBuilder {
    pub async fn build_file(
        &self,
        statements: impl IntoIterator<Item = parse::Statement> + Send,
    ) -> Result<Vec<StatementWithAttributes<Statement>>, SyntaxError> {
        stream::iter(statements)
            .then(|statement| {
                self.build_statement::<StatementSyntax>(
                    StatementSyntaxContext::new(self.clone()),
                    statement,
                )
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .flatten()
            .collect()
    }

    async fn build_statement<S: Syntax>(
        &self,
        context: S::Context,
        statement: parse::Statement,
    ) -> Option<Result<StatementWithAttributes<<S::Context as SyntaxContext>::Body>, SyntaxError>>
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

        let statement = match self
            .build_list::<S>(context.clone(), span, &exprs)
            .await
            .unwrap_or_else(|| context.build_terminal(parse::Expr::list(span, exprs)))
        {
            Ok(statement) => statement,
            Err(error) => return Some(Err(error)),
        };

        Some(Ok(StatementWithAttributes {
            attributes: attributes.into_unique(),
            statement,
        }))
    }

    async fn build_expr<S: Syntax>(
        &self,
        context: S::Context,
        expr: parse::Expr,
    ) -> Result<<S::Context as SyntaxContext>::Body, SyntaxError> {
        match expr.kind {
            parse::ExprKind::Block(statements) => context.build_block(expr.span, statements).await,
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
