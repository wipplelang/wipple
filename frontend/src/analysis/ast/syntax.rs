use crate::{
    analysis::ast::{AstBuilder, StatementAttributes},
    diagnostics::Note,
    helpers::{Backtrace, Shared},
    parse, ScopeId,
};
use async_trait::async_trait;
use futures::{future::BoxFuture, Future};
use std::collections::VecDeque;

pub(in crate::analysis::ast) trait Syntax
where
    Self: 'static,
{
    type Context: SyntaxContext;

    fn rules() -> SyntaxRules<Self>;
}

#[async_trait]
pub(in crate::analysis::ast) trait SyntaxContext: Clone + Send {
    type Body: Send + 'static;
    type Statement: Syntax;

    const PREFERS_LISTS: bool = false;

    fn new(ast_builder: AstBuilder) -> Self;

    fn with_statement_attributes(self, attributes: Shared<StatementAttributes>) -> Self;

    fn block_scope(&self, scope: ScopeId) -> ScopeId {
        scope
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
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError>;

    /// Build an expression that contains no syntaxes or operators.
    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError>;
}

#[derive(Debug, Clone)]
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

impl Syntax for std::convert::Infallible {
    type Context = std::convert::Infallible;

    fn rules() -> SyntaxRules<Self> {
        unreachable!()
    }
}

#[async_trait]
impl SyntaxContext for std::convert::Infallible {
    type Body = std::convert::Infallible;
    type Statement = std::convert::Infallible;

    fn new(_ast_builder: AstBuilder) -> Self {
        unreachable!()
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes>) -> Self {
        unreachable!()
    }

    async fn build_block(
        self,
        _span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        unreachable!()
    }

    async fn build_terminal(
        self,
        _expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        unreachable!()
    }
}

pub(in crate::analysis::ast) struct SyntaxRule<S: Syntax + ?Sized> {
    pub name: &'static str,
    pub kind: SyntaxRuleKind<S>,
}

pub(in crate::analysis::ast) enum SyntaxRuleKind<S: Syntax + ?Sized> {
    Function(
        Box<
            dyn Fn(
                    S::Context,
                    parse::Span,
                    Vec<parse::Expr>,
                    ScopeId,
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
                    parse::Span,
                    (parse::Span, Vec<parse::Expr>),
                    ScopeId,
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
        rule: impl Fn(S::Context, parse::Span, Vec<parse::Expr>, ScopeId) -> Fut + Send + Sync + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            kind: SyntaxRuleKind::Function(Box::new(move |context, span, expr, scope| {
                Some(Box::pin(rule(context, span, expr, scope)))
            })),
        }
    }

    pub fn operator<
        Fut: Future<Output = Result<<S::Context as SyntaxContext>::Body, SyntaxError>> + Send + 'static,
    >(
        name: &'static str,
        associativity: OperatorAssociativity,
        rule: impl Fn(
                S::Context,
                (parse::Span, Vec<parse::Expr>),
                parse::Span,
                (parse::Span, Vec<parse::Expr>),
                ScopeId,
            ) -> Fut
            + Send
            + Sync
            + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            kind: SyntaxRuleKind::Operator(
                associativity,
                Box::new(move |context, lhs, span, rhs, scope| {
                    Some(Box::pin(rule(context, lhs, span, rhs, scope)))
                }),
            ),
        }
    }
}

pub(in crate::analysis::ast) struct SyntaxRules<S: Syntax + ?Sized>(Vec<SyntaxRule<S>>);

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
                    SyntaxRuleKind::<S>::Function(Box::new(move |context, span, expr, scope| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, span, expr, scope)?;
                        Some(Box::pin(async { fut.await.map(From::from) }))
                    }))
                }
                SyntaxRuleKind::Operator(associativity, apply) => SyntaxRuleKind::<S>::Operator(
                    associativity,
                    Box::new(move |context, lhs, span, rhs, scope| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, lhs, span, rhs, scope)?;
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
    pub(in crate::analysis::ast) async fn build_list<S: Syntax>(
        &self,
        context: S::Context,
        span: parse::Span,
        mut exprs: Vec<parse::Expr>,
        scope: ScopeId,
    ) -> Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>> {
        while exprs.len() == 1 {
            let expr = exprs.pop().unwrap();

            match expr.try_into_list_exprs() {
                Ok((_, list_exprs)) => {
                    exprs = list_exprs.collect();
                }
                Err(expr) => {
                    exprs.push(expr);
                    break;
                }
            }
        }

        for rule in S::rules().0 {
            let context = context.clone();

            match rule.kind {
                SyntaxRuleKind::Function(apply) => {
                    if let Some(fut) =
                        self.apply_function_syntax::<S>(rule.name, apply, context, &exprs, scope)
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
                        &exprs,
                        scope,
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
                parse::Span,
                Vec<parse::Expr>,
                ScopeId,
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
            > + Send
            + Sync
            + 'static,
        context: S::Context,
        exprs: &[parse::Expr],
        scope: ScopeId,
    ) -> Option<BoxFuture<Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>>>> {
        let mut exprs = exprs.iter();

        let first = exprs.next()?;

        if let parse::ExprKind::Name(first_name, _) = &first.kind {
            if first_name.as_str() == name {
                let span = first.span;
                let exprs = exprs.cloned().collect();

                return Some(Box::pin(async move {
                    Some(apply(context, span, exprs, scope)?.await)
                }));
            }
        }

        None
    }

    fn apply_operator_syntax<S: Syntax + ?Sized>(
        &self,
        name: &'static str,
        associativity: OperatorAssociativity,
        apply: impl Fn(
                S::Context,
                (parse::Span, Vec<parse::Expr>),
                parse::Span,
                (parse::Span, Vec<parse::Expr>),
                ScopeId,
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext>::Body, SyntaxError>>,
            > + Send
            + Sync
            + 'static,
        context: S::Context,
        span: parse::Span,
        exprs: &[parse::Expr],
        scope: ScopeId,
    ) -> Option<BoxFuture<Option<Result<<S::Context as SyntaxContext>::Body, SyntaxError>>>> {
        if exprs.is_empty() {
            return None;
        }

        let mut occurrences = VecDeque::new();

        for (index, expr) in exprs.iter().enumerate() {
            if let parse::ExprKind::Name(expr_name, _) = expr.kind {
                if expr_name.as_str() == name {
                    occurrences.push_back(index);
                }
            }
        }

        if let OperatorAssociativity::Variadic = associativity {
            let operator_span = exprs[*occurrences.front()?].span;

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

                    Ok(parse::Expr::list_or_expr(
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
                Some(
                    apply(
                        context,
                        (span, exprs),
                        operator_span,
                        (parse::Span::builtin(), Vec::new()),
                        scope,
                    )?
                    .await,
                )
            }));
        }

        let occurrences = Vec::from(occurrences);

        let index = match associativity {
            OperatorAssociativity::Left => occurrences.last()?,
            OperatorAssociativity::Right => occurrences.first()?,
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
            Some(
                apply(
                    context,
                    (lhs_span, lhs),
                    operator_span,
                    (rhs_span, rhs),
                    scope,
                )?
                .await,
            )
        }))
    }
}

pub struct ErrorSyntax;

#[derive(Clone)]
pub struct ErrorSyntaxContext {
    ast_builder: AstBuilder,
}

impl Syntax for ErrorSyntax {
    type Context = ErrorSyntaxContext;

    fn rules() -> SyntaxRules<Self> {
        SyntaxRules::new()
    }
}

#[async_trait]
impl SyntaxContext for ErrorSyntaxContext {
    type Body = std::convert::Infallible;
    type Statement = std::convert::Infallible;

    fn new(ast_builder: AstBuilder) -> Self {
        ErrorSyntaxContext { ast_builder }
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes>) -> Self {
        self
    }

    async fn build_block(
        self,
        span: parse::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Err(self.ast_builder.syntax_error(expr.span))
    }
}