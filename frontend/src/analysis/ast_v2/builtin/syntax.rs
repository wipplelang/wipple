use crate::{
    analysis::ast_v2::AstBuilder,
    diagnostics::Note,
    helpers::{Backtrace, Shared},
    parse,
};
use async_trait::async_trait;
use futures::{future::BoxFuture, Future};
use std::collections::VecDeque;

pub(super) trait Syntax
where
    Self: 'static,
{
    type Context: SyntaxContext;

    fn rules() -> SyntaxRules<Self>;
}

#[async_trait]
pub(super) trait SyntaxContext: Clone + Send {
    type Body: Send + 'static;

    fn new(ast_builder: AstBuilder) -> Self;

    async fn build_block(
        self,
        span: parse::Span,
        statements: impl IntoIterator<Item = parse::Statement> + Send,
    ) -> Result<Self::Body, SyntaxError>;

    /// Build an expression that contains no syntaxes or operators.
    fn build_terminal(self, expr: parse::Expr) -> Result<Self::Body, SyntaxError>;
}

pub(super) trait FileBodySyntaxContext: SyntaxContext {
    fn with_statement_attributes(self, attributes: Shared<Vec<()> /* TODO */>) -> Self;
}

pub struct SyntaxError {
    pub span: parse::Span,
    pub trace: Backtrace,
}

impl AstBuilder {
    pub fn syntax_error(&self, span: parse::Span) -> SyntaxError {
        SyntaxError {
            span,
            trace: self.compiler.backtrace(),
        }
    }
}

pub(super) struct SyntaxRule<S: Syntax + ?Sized> {
    pub name: &'static str,
    pub kind: SyntaxRuleKind<S>,
}

pub(super) enum SyntaxRuleKind<S: Syntax + ?Sized> {
    Function(
        Box<
            dyn Fn(
                    S::Context,
                    Vec<parse::Expr>,
                ) -> Option<
                    BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
                > + Send
                + Sync,
        >,
    ),
    Operator(
        OperatorAssociativity,
        Box<
            dyn Fn(
                    S::Context,
                    (parse::Span, Vec<parse::Expr>),
                    (parse::Span, Vec<parse::Expr>),
                ) -> Option<
                    BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
                > + Send
                + Sync,
        >,
    ),
}

impl<S: Syntax + ?Sized> SyntaxRule<S> {
    pub fn function<
        Fut: Future<Output = Result<<S::Context as SyntaxContext>::Body, SyntaxError>> + Send + 'static,
    >(
        name: &'static str,
        rule: impl Fn(S::Context, Vec<parse::Expr>) -> Fut + Send + Sync + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            kind: SyntaxRuleKind::Function(Box::new(move |context, expr| {
                Some(Box::pin(rule(context, expr)))
            })),
        }
    }

    pub fn operator<
        Fut: Future<Output = Result<<S::Context as SyntaxContext>::Body, SyntaxError>> + Send + 'static,
    >(
        name: &'static str,
        associativity: OperatorAssociativity,
        rule: impl Fn(S::Context, (parse::Span, Vec<parse::Expr>), (parse::Span, Vec<parse::Expr>)) -> Fut
            + Send
            + Sync
            + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            kind: SyntaxRuleKind::Operator(
                associativity,
                Box::new(move |context, lhs, rhs| Some(Box::pin(rule(context, lhs, rhs)))),
            ),
        }
    }
}

pub(super) struct SyntaxRules<S: Syntax + ?Sized>(Vec<SyntaxRule<S>>);

impl<S: Syntax + ?Sized> Default for SyntaxRules<S> {
    fn default() -> Self {
        SyntaxRules(Vec::new())
    }
}

impl<S: Syntax + ?Sized> SyntaxRules<S> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(mut self, rule: SyntaxRule<S>) -> Self {
        self.0.push(rule);
        self
    }

    pub fn combine<S2: Syntax + ?Sized + 'static>(mut self, rules: SyntaxRules<S2>) -> Self
    where
        S2::Context: TryFrom<S::Context>,
        <S::Context as SyntaxContext>::Body: From<<S2::Context as SyntaxContext>::Body>,
    {
        self.0.extend(rules.0.into_iter().map(|rule| SyntaxRule {
            name: rule.name,
            kind: match rule.kind {
                SyntaxRuleKind::Function(apply) => {
                    SyntaxRuleKind::<S>::Function(Box::new(move |context, expr| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, expr)?;
                        Some(Box::pin(async { fut.await.map(From::from) }))
                    }))
                }
                SyntaxRuleKind::Operator(associativity, apply) => SyntaxRuleKind::<S>::Operator(
                    associativity,
                    Box::new(move |context, lhs, rhs| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, lhs, rhs)?;
                        Some(Box::pin(async { fut.await.map(From::from) }))
                    }),
                ),
            },
        }));

        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperatorAssociativity {
    Left,
    Right,
    Variadic,
    None,
}

impl AstBuilder {
    pub(super) async fn build_list<S: Syntax>(
        &self,
        context: S::Context,
        span: parse::Span,
        exprs: &[parse::Expr],
    ) -> Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>> {
        for rule in S::rules().0 {
            let context = context.clone();

            match rule.kind {
                SyntaxRuleKind::Function(apply) => {
                    if let Some(fut) =
                        self.apply_function_syntax::<S>(rule.name, apply, context, span, exprs)
                    {
                        if let Some(result) = fut.await {
                            return Some(result);
                        }
                    }
                }
                SyntaxRuleKind::Operator(associativity, apply) => {
                    if let Some(fut) = self.apply_operator_syntax::<S>(
                        rule.name,
                        associativity,
                        apply,
                        context,
                        span,
                        exprs,
                    ) {
                        if let Some(result) = fut.await {
                            return Some(result);
                        }
                    }
                }
            }
        }

        None
    }

    fn apply_function_syntax<S: Syntax + ?Sized>(
        &self,
        name: &'static str,
        apply: impl Fn(
                S::Context,
                Vec<parse::Expr>,
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
            > + Send
            + Sync
            + 'static,
        context: S::Context,
        span: parse::Span,
        exprs: &[parse::Expr],
    ) -> Option<BoxFuture<Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>>>> {
        todo!()
    }

    fn apply_operator_syntax<S: Syntax + ?Sized>(
        &self,
        name: &'static str,
        associativity: OperatorAssociativity,
        apply: impl Fn(
                S::Context,
                (parse::Span, Vec<parse::Expr>),
                (parse::Span, Vec<parse::Expr>),
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
            > + Send
            + Sync
            + 'static,
        context: S::Context,
        span: parse::Span,
        exprs: &[parse::Expr],
    ) -> Option<BoxFuture<Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>>>> {
        let mut occurrences = VecDeque::new();

        for (index, expr) in exprs.iter().enumerate() {
            if let parse::ExprKind::Name(expr_name) = expr.kind {
                if expr_name.as_str() == name {
                    occurrences.push_back(index);
                }
            }
        }

        if let OperatorAssociativity::Variadic = associativity {
            let mut grouped_exprs = vec![(None, Vec::new())];
            {
                let mut operators = occurrences.clone();

                for (index, expr) in exprs.iter().enumerate() {
                    let operator_index = operators.front().copied();

                    if Some(index) == operator_index {
                        operators.pop_front();
                        grouped_exprs.push((Some(index), Vec::new()));
                    } else {
                        grouped_exprs.last_mut().unwrap().1.push(expr);
                    }
                }
            }

            // Allow trailing operators
            if grouped_exprs.last().unwrap().1.is_empty() {
                grouped_exprs.pop();
            }

            let result = grouped_exprs
                .into_iter()
                .map(|(operator_index, grouped_exprs)| {
                    if grouped_exprs.is_empty() {
                        if let Some(operator_index) = operator_index {
                            let operator_span = exprs[operator_index].span;

                            self.compiler.add_error(
                                "expected values on right side of operator",
                                vec![Note::primary(
                                    operator_span,
                                    "try providing a value to the right of this",
                                )],
                            );
                        } else {
                            let operator_span = exprs[*occurrences.front().unwrap()].span;

                            self.compiler.add_error(
                                "expected values on left side of operator",
                                vec![Note::primary(
                                    operator_span,
                                    "try providing a value to the left of this",
                                )],
                            );
                        }

                        let error = self.syntax_error(span);
                        return Err(error);
                    }

                    let span = parse::Span::join(
                        grouped_exprs.first().unwrap().span,
                        grouped_exprs.last().unwrap().span,
                    );

                    Ok(parse::Expr::list(
                        span,
                        grouped_exprs.into_iter().cloned().collect(),
                    ))
                })
                .collect::<Result<Vec<_>, _>>();

            let exprs = match result {
                Ok(exprs) => exprs,
                Err(error) => return Some(Box::pin(async move { Some(Err(error)) })),
            };

            debug_assert!(!exprs.is_empty());

            // HACK: Put all the expressions into `lhs`. In the future, handle
            // variadic operators specially.

            let span = parse::Span::join(exprs.first().unwrap().span, exprs.last().unwrap().span);

            return Some(Box::pin(async move {
                Some(apply(context, (span, exprs), (parse::Span::builtin(), Vec::new()))?.await)
            }));
        }

        let occurrences = Vec::from(occurrences);

        let index = match associativity {
            OperatorAssociativity::Left => occurrences.first()?,
            OperatorAssociativity::Right => occurrences.last()?,
            OperatorAssociativity::Variadic => unreachable!("handled above"),
            OperatorAssociativity::None => {
                if occurrences.len() <= 1 {
                    occurrences.first()?
                } else {
                    let (first, rest) = occurrences.split_first().unwrap();

                    for occurrence in rest {
                        self.compiler.add_error(
                            "operator ambiguity",
                            vec![
                                Note::primary(
                                    exprs[*occurrence].span,
                                    "only one of this operator may be provided at a time",
                                ),
                                Note::secondary(exprs[*first].span, "first use of this operator"),
                            ],
                        );
                    }

                    let error = self.syntax_error(span);
                    return Some(Box::pin(async move { Some(Err(error)) }));
                }
            }
        };

        let operator_span = exprs[*index].span;

        let mut exprs = exprs.to_vec();
        let rhs = exprs.split_off(index + 1);
        let mut lhs = exprs;
        lhs.pop().unwrap();

        if rhs.is_empty() {
            self.compiler.add_error(
                "expected values on right side of operator",
                vec![Note::primary(
                    operator_span,
                    "try providing a value to the right of this",
                )],
            );

            let error = self.syntax_error(span);
            return Some(Box::pin(async move { Some(Err(error)) }));
        } else if lhs.is_empty() {
            self.compiler.add_error(
                "expected values on left side of operator",
                vec![Note::primary(
                    operator_span,
                    "try providing a value to the left of this",
                )],
            );

            let error = self.syntax_error(span);
            return Some(Box::pin(async move { Some(Err(error)) }));
        }

        let lhs_span = parse::Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span);
        let rhs_span = parse::Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span);

        Some(Box::pin(async move {
            Some(apply(context, (lhs_span, lhs), (rhs_span, rhs))?.await)
        }))
    }
}
