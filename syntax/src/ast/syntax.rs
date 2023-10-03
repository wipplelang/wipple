#![allow(clippy::too_many_arguments, clippy::type_complexity)]

use crate::{
    ast::{AstBuilder, StatementAttributes},
    parse, Driver, File, Fix, FixRange, ScopeSet, Span,
};
use async_trait::async_trait;
use futures::{future::BoxFuture, Future};
use std::collections::VecDeque;
use wipple_util::{Backtrace, Shared};

pub(crate) trait Syntax<D: Driver>: Send + Sync + 'static {
    type Context: SyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self>;
}

#[async_trait]
pub(crate) trait SyntaxContext<D: Driver>: Clone + Send + Sync {
    type Body: Send;
    type Statement: Syntax<D>;

    const PREFERS_LISTS: bool = false;
    const BLOCK_CREATES_SCOPE: bool = false;

    fn new(ast_builder: AstBuilder<D>) -> Self;

    fn with_statement_attributes(self, attributes: Shared<StatementAttributes<D>>) -> Self;

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>>;

    /// Build an expression that contains no syntaxes or operators.
    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>>;
}

#[derive(Debug, Clone)]
pub struct SyntaxError<D: Driver> {
    pub span: D::Span,
    pub trace: Backtrace,
}

impl<D: Driver> AstBuilder<D> {
    pub fn syntax_error(&self, span: D::Span) -> SyntaxError<D> {
        SyntaxError {
            span,
            trace: self.driver.backtrace(),
        }
    }
}

impl<D: Driver> Syntax<D> for parse::Expr<D> {
    type Context = RawSyntaxContext;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules(Vec::new())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RawSyntaxContext;

#[async_trait]
impl<D: Driver> SyntaxContext<D> for RawSyntaxContext {
    type Body = parse::Expr<D>;
    type Statement = parse::Expr<D>;

    fn new(_ast_builder: AstBuilder<D>) -> Self {
        RawSyntaxContext
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes<D>>) -> Self {
        unimplemented!()
    }

    async fn build_block(
        self,
        span: D::Span,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(parse::Expr::new(
            span,
            parse::ExprKind::Block(
                statements
                    .map(|statement| parse::Statement {
                        lines: vec![parse::ListLine::from(vec![
                            statement.unwrap_or_else(|_| unreachable!())
                        ])],
                    })
                    .collect(),
            ),
        ))
    }

    /// Build an expression that contains no syntaxes or operators.
    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(expr)
    }
}

impl<D: Driver> Syntax<D> for std::convert::Infallible {
    type Context = std::convert::Infallible;

    fn rules() -> SyntaxRules<D, Self> {
        unreachable!()
    }
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for std::convert::Infallible {
    type Body = std::convert::Infallible;
    type Statement = std::convert::Infallible;

    fn new(_ast_builder: AstBuilder<D>) -> Self {
        unreachable!()
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes<D>>) -> Self {
        unreachable!()
    }

    async fn build_block(
        self,
        _span: D::Span,
        _statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax<D>>::Context as SyntaxContext<D>>::Body,
                    SyntaxError<D>,
                >,
            > + Send,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        unreachable!()
    }

    async fn build_terminal(
        self,
        _expr: parse::Expr<D>,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        unreachable!()
    }
}

pub(crate) struct SyntaxRule<D: Driver, S: Syntax<D> + ?Sized> {
    pub name: &'static str,
    pub shallow: bool,
    pub kind: SyntaxRuleKind<D, S>,
}

pub(crate) enum SyntaxRuleKind<D: Driver, S: Syntax<D> + ?Sized> {
    Function(
        Box<
            dyn Fn(
                    S::Context,
                    D::Span,
                    D::Span,
                    Vec<parse::Expr<D>>,
                    Shared<ScopeSet<D::Scope>>,
                ) -> Option<
                    BoxFuture<
                        'static,
                        Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>,
                    >,
                > + Send
                + Sync,
        >,
    ),
    Operator(
        OperatorAssociativity,
        Box<
            dyn Fn(
                    S::Context,
                    D::Span,
                    (D::Span, Vec<parse::Expr<D>>),
                    D::Span,
                    (D::Span, Vec<parse::Expr<D>>),
                    Shared<ScopeSet<D::Scope>>,
                ) -> Option<
                    BoxFuture<
                        'static,
                        Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>,
                    >,
                > + Send
                + Sync,
        >,
    ),
}

impl<D: Driver, S: Syntax<D>> SyntaxRule<D, S> {
    pub fn function<
        Fut: Future<Output = Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>
            + Send
            + Sync
            + 'static,
    >(
        name: &'static str,
        rule: impl Fn(S::Context, D::Span, D::Span, Vec<parse::Expr<D>>, Shared<ScopeSet<D::Scope>>) -> Fut
            + Send
            + Sync
            + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            shallow: false,
            kind: SyntaxRuleKind::Function(Box::new(
                move |context, span, syntax_span, expr, scope| {
                    Some(Box::pin(rule(context, span, syntax_span, expr, scope)))
                },
            )),
        }
    }

    pub fn operator<
        Fut: Future<Output = Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>
            + Send
            + Sync
            + 'static,
    >(
        name: &'static str,
        associativity: OperatorAssociativity,
        rule: impl Fn(
                S::Context,
                D::Span,
                (D::Span, Vec<parse::Expr<D>>),
                D::Span,
                (D::Span, Vec<parse::Expr<D>>),
                Shared<ScopeSet<D::Scope>>,
            ) -> Fut
            + Send
            + Sync
            + 'static,
    ) -> Self {
        SyntaxRule {
            name,
            shallow: false,
            kind: SyntaxRuleKind::Operator(
                associativity,
                Box::new(move |context, span, lhs, syntax_span, rhs, scope| {
                    Some(Box::pin(rule(context, span, lhs, syntax_span, rhs, scope)))
                }),
            ),
        }
    }
}

pub(crate) struct SyntaxRules<D: Driver, S: Syntax<D> + ?Sized>(Vec<SyntaxRule<D, S>>);

impl<D: Driver, S: Syntax<D> + ?Sized> Default for SyntaxRules<D, S> {
    fn default() -> Self {
        SyntaxRules(Vec::new())
    }
}

impl<D: Driver, S: Syntax<D> + ?Sized> SyntaxRules<D, S> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with(mut self, rule: SyntaxRule<D, S>) -> Self {
        self.0.push(rule);
        self
    }

    pub fn shallow(mut self) -> Self {
        for rule in &mut self.0 {
            rule.shallow = true;
        }

        self
    }

    pub fn combine<S2: Syntax<D> + ?Sized>(mut self, rules: SyntaxRules<D, S2>) -> Self
    where
        S2::Context: TryFrom<S::Context>,
        <S::Context as SyntaxContext<D>>::Body: From<<S2::Context as SyntaxContext<D>>::Body>,
    {
        self.0.extend(rules.0.into_iter().map(|rule| SyntaxRule {
            name: rule.name,
            shallow: rule.shallow,
            kind: match rule.kind {
                SyntaxRuleKind::Function(apply) => SyntaxRuleKind::<D, S>::Function(Box::new(
                    move |context, span, syntax_span, expr, scope| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, span, syntax_span, expr, scope)?;
                        Some(Box::pin(async { fut.await.map(From::from) }))
                    },
                )),
                SyntaxRuleKind::Operator(associativity, apply) => SyntaxRuleKind::<D, S>::Operator(
                    associativity,
                    Box::new(move |context, span, lhs, operator_span, rhs, scope| {
                        let context = context.try_into().ok()?;
                        let fut = apply(context, span, lhs, operator_span, rhs, scope)?;
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

impl<D: Driver> AstBuilder<D> {
    fn make_into_list(&self, mut exprs: Vec<parse::Expr<D>>) -> Vec<parse::Expr<D>> {
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

        exprs
    }

    pub(crate) fn list_matches_syntax<S: Syntax<D>>(&self, exprs: Vec<parse::Expr<D>>) -> bool {
        let exprs = self.make_into_list(exprs);

        for rule in S::rules().0 {
            if rule.shallow {
                continue;
            }

            match rule.kind {
                SyntaxRuleKind::Function(_) => {
                    if self.match_function_syntax::<S>(rule.name, &exprs).is_some() {
                        return true;
                    }
                }
                SyntaxRuleKind::Operator(_, _) => {
                    if self.match_operator_syntax::<S>(rule.name, &exprs).is_some() {
                        return true;
                    }
                }
            }
        }

        false
    }

    pub(crate) async fn build_list<S: Syntax<D>>(
        &self,
        context: S::Context,
        span: D::Span,
        exprs: Vec<parse::Expr<D>>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Option<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>> {
        let exprs = self.make_into_list(exprs);

        let prev_scope_set = scope_set.lock().clone();

        for rule in S::rules().0 {
            let context = context.clone();

            match rule.kind {
                SyntaxRuleKind::Function(apply) => {
                    if let Some(fut) = self.apply_function_syntax::<S>(
                        rule.name,
                        apply,
                        context,
                        span,
                        &exprs,
                        scope_set.clone(),
                    ) {
                        if let Some(result) = fut.await {
                            return Some(result);
                        }

                        *scope_set.lock() = prev_scope_set.clone();
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
                        scope_set.clone(),
                    ) {
                        if let Some(result) = fut.await {
                            return Some(result);
                        }

                        *scope_set.lock() = prev_scope_set.clone();
                    }
                }
            }
        }

        None
    }

    fn match_function_syntax<S: Syntax<D> + ?Sized>(
        &self,
        name: &'static str,
        exprs: &[parse::Expr<D>],
    ) -> Option<(D::Span, Vec<parse::Expr<D>>)> {
        let mut exprs = exprs.iter();
        let first = exprs.next()?;

        if let parse::ExprKind::Name(first_name, _) = &first.kind {
            if first_name.as_ref() == name {
                return Some((first.span, exprs.cloned().collect()));
            }
        }

        None
    }

    fn apply_function_syntax<'a, S: Syntax<D> + ?Sized>(
        &'a self,
        name: &'static str,
        apply: impl Fn(
                S::Context,
                D::Span,
                D::Span,
                Vec<parse::Expr<D>>,
                Shared<ScopeSet<D::Scope>>,
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>,
            > + Send
            + Sync
            + 'a,
        context: S::Context,
        span: D::Span,
        exprs: &[parse::Expr<D>],
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Option<BoxFuture<'a, Option<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>>>
    {
        if let Some((syntax_span, exprs)) = self.match_function_syntax::<S>(name, exprs) {
            return Some(Box::pin(async move {
                Some(match apply(context, span, syntax_span, exprs, scope_set) {
                    Some(result) => {
                        self.file.use_builtin_syntax(syntax_span, name);
                        result.await
                    }
                    None => return None,
                })
            }));
        }

        None
    }

    fn match_operator_syntax<S: Syntax<D> + ?Sized>(
        &self,
        name: &'static str,
        exprs: &[parse::Expr<D>],
    ) -> Option<VecDeque<usize>> {
        if exprs.is_empty() {
            return None;
        }

        let mut occurrences = VecDeque::new();

        for (index, expr) in exprs.iter().enumerate() {
            if let parse::ExprKind::Name(expr_name, _) = &expr.kind {
                if expr_name.as_ref() == name {
                    occurrences.push_back(index);
                }
            }
        }

        (!occurrences.is_empty()).then_some(occurrences)
    }

    fn apply_operator_syntax<'a, S: Syntax<D> + ?Sized>(
        &'a self,
        name: &'static str,
        associativity: OperatorAssociativity,
        apply: impl Fn(
                S::Context,
                D::Span,
                (D::Span, Vec<parse::Expr<D>>),
                D::Span,
                (D::Span, Vec<parse::Expr<D>>),
                Shared<ScopeSet<D::Scope>>,
            ) -> Option<
                BoxFuture<'static, Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>,
            > + Send
            + Sync
            + 'static,
        context: S::Context,
        span: D::Span,
        exprs: &[parse::Expr<D>],
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Option<BoxFuture<'a, Option<Result<<S::Context as SyntaxContext<D>>::Body, SyntaxError<D>>>>>
    {
        let occurrences = self.match_operator_syntax::<S>(name, exprs)?;

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

                            self.driver.syntax_error_with(
                                [(
                                    operator_span,
                                    format!("expected values on right side of `{name}`"),
                                )],
                                Some(Fix::new(
                                    format!("insert a value to the right of `{name}`"),
                                    FixRange::after(operator_span),
                                    " {%value%}",
                                )),
                            );
                        } else {
                            let operator_span = exprs[*occurrences.front().unwrap()].span;

                            self.driver.syntax_error_with(
                                [(
                                    operator_span,
                                    format!("expected values on left side of `{name}`"),
                                )],
                                Some(Fix::new(
                                    format!("insert a value to the left of `{name}"),
                                    FixRange::before(operator_span),
                                    "{%value%} ",
                                )),
                            );
                        }

                        let error = self.syntax_error(span);
                        return Err(error);
                    }

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

            return Some(Box::pin(async move {
                Some(
                    match apply(
                        context,
                        span,
                        (span, exprs),
                        operator_span,
                        (operator_span, Vec::new()),
                        scope_set,
                    ) {
                        Some(result) => {
                            self.file.use_builtin_syntax(operator_span, name);
                            result.await
                        }
                        None => return None,
                    },
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
                        self.driver.syntax_error_with(
                            [
                                (
                                    exprs[*occurrence].span,
                                    format!("only one of `{name}` may be provided at a time"),
                                ),
                                (exprs[*first].span, format!("first use of `{name}`")),
                            ],
                            Some(Fix::new(
                                format!("remove this use of `{name}`"),
                                FixRange::replace(exprs[*occurrence].span),
                                "",
                            )),
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

        if lhs.is_empty() {
            self.driver.syntax_error_with(
                [(
                    operator_span,
                    format!("expected values on left side of `{name}`"),
                )],
                Some(Fix::new(
                    format!("insert a value to the left of `{name}`"),
                    FixRange::before(operator_span),
                    "{%value%} ",
                )),
            );

            let error = self.syntax_error(span);
            return Some(Box::pin(async move { Some(Err(error)) }));
        } else if rhs.is_empty() {
            self.driver.syntax_error_with(
                [(
                    operator_span,
                    format!("expected values on right side of `{name}`"),
                )],
                Some(Fix::new(
                    format!("insert a value to the right of `{name}`"),
                    FixRange::after(operator_span),
                    " {%value%}",
                )),
            );

            let error = self.syntax_error(span);
            return Some(Box::pin(async move { Some(Err(error)) }));
        }

        let lhs_span = Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span);
        let rhs_span = Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span);

        Some(Box::pin(async move {
            Some(
                match apply(
                    context,
                    span,
                    (lhs_span, lhs),
                    operator_span,
                    (rhs_span, rhs),
                    scope_set,
                ) {
                    Some(result) => {
                        self.file.use_builtin_syntax(operator_span, name);
                        result.await
                    }
                    None => return None,
                },
            )
        }))
    }
}

pub struct ErrorSyntax;

#[derive(Clone)]
pub struct ErrorSyntaxContext<D: Driver> {
    ast_builder: AstBuilder<D>,
}

impl<D: Driver> Syntax<D> for ErrorSyntax {
    type Context = ErrorSyntaxContext<D>;

    fn rules() -> SyntaxRules<D, Self> {
        SyntaxRules::new()
    }
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for ErrorSyntaxContext<D> {
    type Body = std::convert::Infallible;
    type Statement = std::convert::Infallible;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        ErrorSyntaxContext { ast_builder }
    }

    fn with_statement_attributes(self, _attributes: Shared<StatementAttributes<D>>) -> Self {
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
        Err(self.ast_builder.syntax_error(span))
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        _scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Err(self.ast_builder.syntax_error(expr.span))
    }
}
