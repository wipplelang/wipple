#[macro_use]
mod macros;

mod assignment_pattern;
mod assignment_value;
mod expression;
mod file_attribute;
mod pattern;
mod statement;
mod statement_attribute;
mod r#type;

use assignment_pattern::*;
// use assignment_value::*;
// use expression::*;
// use file_attribute::*;
// use pattern::*;
// use r#type::*;
// use statement::*;
// use statement_attribute::*;

use crate::parse;
use futures::{future::BoxFuture, Future};

syntax_context_group! {
    pub enum SyntaxContext {
        AssignmentPattern,
    }
}

root_syntax_group! {
    pub enum AnySyntax<SyntaxContext> {
        AssignmentPattern,
    }
}

pub trait Syntax<'a> {
    type Context;
    type Body;

    fn rules() -> SyntaxRules<'a, Self>;
}

pub struct SyntaxRule<'a, Context, Body>(
    Box<dyn Fn(Context, Vec<parse::Expr>) -> Option<BoxFuture<'a, Body>> + 'a>,
);

impl<'a, Context, Body> SyntaxRule<'a, Context, Body> {
    fn new<Fut: Future<Output = Body> + Send + 'a>(
        rule: impl Fn(Context, Vec<parse::Expr>) -> Option<Fut> + 'a,
    ) -> Self {
        SyntaxRule(Box::new(move |context, expr| {
            Some(Box::pin(rule(context, expr)?))
        }))
    }

    fn apply(&self, context: Context, exprs: Vec<parse::Expr>) -> Option<BoxFuture<'a, Body>> {
        (self.0)(context, exprs)
    }
}

pub struct SyntaxRules<'a, S: Syntax<'a> + ?Sized>(Vec<SyntaxRule<'a, S::Context, S::Body>>);

impl<'a, S: Syntax<'a> + ?Sized> Default for SyntaxRules<'a, S> {
    fn default() -> Self {
        SyntaxRules(Vec::new())
    }
}

impl<'a, S: Syntax<'a> + ?Sized> SyntaxRules<'a, S> {
    fn new() -> Self {
        Default::default()
    }

    fn with(mut self, rule: SyntaxRule<'a, S::Context, S::Body>) -> Self {
        self.0.push(rule);
        self
    }

    fn combine<S2: Syntax<'a> + ?Sized + 'static>(mut self, rules: SyntaxRules<'a, S2>) -> Self
    where
        S2::Context: TryFrom<S::Context>,
        S::Body: From<S2::Body>,
    {
        self.0.extend(rules.0.into_iter().map(|rule| {
            SyntaxRule::<S::Context, S::Body>::new(move |context, expr| {
                let context = context.try_into().ok()?;
                let fut = rule.apply(context, expr)?;
                Some(Box::pin(async { fut.await.into() }))
            })
        }));

        self
    }
}
