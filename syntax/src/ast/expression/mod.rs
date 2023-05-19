definitions! {
    mod annotate;
    mod external;
    mod format;
    mod function;
    mod tuple;
    mod when;
    mod with;
}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, OperatorAssociativity, OperatorPrecedenceStatementAttributeKind, Statement,
        StatementAttributes, StatementSyntax, SyntaxAssignmentValue, SyntaxBody, SyntaxPattern,
        SyntaxRule,
    },
    parse, Driver, File, Span,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use std::{cmp::Ordering, collections::VecDeque};
use wipple_util::Shared;

syntax_group! {
    pub type Expression<ExpressionSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
            Annotate,
            External,
            Format,
            With,
            When,
        },
        terminal: {
            Unit,
            Name,
            Text,
            Number,
            Call,
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitExpression<D: Driver> {
    pub span: D::Span,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for UnitExpression<D> {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(UnitExpression {
            span: Default::default(),
        })
    }
}

impl<D: Driver> UnitExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnitExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("()"))
    }
}

#[derive(Debug, Clone)]
pub struct NameExpression<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NameExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NameExpression {
            span: Default::default(),
            name: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
}

impl<D: Driver> NameExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("{}", self.name.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct TextExpression<D: Driver> {
    pub span: D::Span,
    pub text: D::InternedString,
    pub raw: D::InternedString,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for TextExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(TextExpression {
            span: Default::default(),
            text: arbitrary::Arbitrary::arbitrary(u)?,
            raw: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> TextExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TextExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("\"{}\"", self.raw.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct NumberExpression<D: Driver> {
    pub span: D::Span,
    pub number: D::InternedString,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for NumberExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(NumberExpression {
            span: Default::default(),
            number: crate::FuzzString(u.int_in_range(0..=100)?.to_string()),
        })
    }
}

impl<D: Driver> NumberExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NumberExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("{}", self.number.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression<D: Driver> {
    pub span: D::Span,
    pub function: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub inputs: Vec<Result<Expression<D>, SyntaxError<D>>>,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for CallExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(CallExpression {
            span: Default::default(),
            function: arbitrary::Arbitrary::arbitrary(u)?,
            inputs: arbitrary::Arbitrary::arbitrary(u)?,
        })
    }
}

impl<D: Driver> CallExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for CallExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({}{})",
            self.function?.format()?,
            self.inputs
                .into_iter()
                .map(|input| Ok(format!(" {}", input?.format()?)))
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpression<D: Driver> {
    pub span: D::Span,
    pub statements: Vec<Result<Statement<D>, SyntaxError<D>>>,
    pub scope: D::Scope,
}

#[cfg(feature = "arbitrary")]
impl<'a, D: crate::FuzzDriver> arbitrary::Arbitrary<'a> for BlockExpression<D> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockExpression {
            span: Default::default(),
            statements: arbitrary::Arbitrary::arbitrary(u)?,
            scope: Default::default(),
        })
    }
}

impl<D: Driver> BlockExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for BlockExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "{{\n{}\n}}",
            self.statements
                .into_iter()
                .map(|statement| statement?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join("\n")
        ))
    }
}

#[derive(Clone)]
pub struct ExpressionSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for ExpressionSyntaxContext<D> {
    type Body = Expression<D>;
    type Statement = StatementSyntax;

    const PREFERS_LISTS: bool = true;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        ExpressionSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes<D>>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    fn block_scope(&self, scope: D::Scope) -> D::Scope {
        self.ast_builder.file.make_scope(scope)
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
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(BlockExpression {
            span,
            statements: statements.collect(),
            scope,
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => self.expand_list(span, exprs.collect(), scope).await,
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, _name_scope) => Ok(NameExpression {
                    span: expr.span,
                    name,
                    scope, // TODO: Hygiene (use `name_scope`)
                }
                .into()),
                parse::ExprKind::Text(text, raw) => Ok(TextExpression {
                    span: expr.span,
                    text,
                    raw,
                }
                .into()),
                parse::ExprKind::Number(number) => Ok(NumberExpression {
                    span: expr.span,
                    number,
                }
                .into()),
                parse::ExprKind::Block(_) => {
                    self.ast_builder
                        .build_expr::<ExpressionSyntax>(self.clone(), expr, self.block_scope(scope))
                        .await
                }
                _ => {
                    self.ast_builder
                        .driver
                        .syntax_error(expr.span, "expected expression");

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}

impl<D: Driver> ExpressionSyntaxContext<D> {
    async fn expand_list(
        &self,
        list_span: D::Span,
        mut exprs: Vec<parse::Expr<D>>,
        scope: D::Scope,
    ) -> Result<Expression<D>, SyntaxError<D>> {
        match exprs.len() {
            0 => Ok(UnitExpression { span: list_span }.into()),
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name, name_scope) = &expr.kind {
                    let syntax = self.ast_builder.file.resolve_syntax(
                        expr.span,
                        name.clone(),
                        name_scope.unwrap_or(scope),
                    );

                    if let Some(syntax) = syntax {
                        match syntax.operator_precedence {
                            Some(_) => {
                                self.ast_builder.driver.syntax_error(
                                    expr.span,
                                    "expected values on both sides of operator",
                                );

                                return Err(self.ast_builder.syntax_error(list_span));
                            }
                            None => {
                                return self
                                    .expand_syntax(expr.span, syntax, vec![expr], scope)
                                    .await;
                            }
                        }
                    }
                }

                self.clone().build_terminal(expr, scope).await
            }
            _ => {
                let operators = self.operators_in_list(exprs.iter().enumerate(), scope);

                if operators.is_empty() {
                    // TODO: Remove `[operator]` in favor of this logic
                    for expr in &exprs {
                        if let parse::ExprKind::Name(name, name_scope) = &expr.kind {
                            let syntax = self.ast_builder.file.resolve_syntax(
                                expr.span,
                                name.clone(),
                                name_scope.unwrap_or(scope),
                            );

                            if let Some(syntax) = syntax {
                                return self.expand_syntax(expr.span, syntax, exprs, scope).await;
                            }
                        }
                    }

                    let mut exprs = exprs.into_iter();
                    let first = exprs.next().unwrap();

                    let function = self
                        .ast_builder
                        .build_expr::<ExpressionSyntax>(self.clone(), first, scope)
                        .await;

                    let inputs = stream::iter(exprs)
                        .then(|expr| {
                            self.ast_builder.build_expr::<ExpressionSyntax>(
                                self.clone(),
                                expr,
                                scope,
                            )
                        })
                        .collect::<Vec<_>>()
                        .await;

                    Ok(CallExpression {
                        span: list_span,
                        function: function.map(Box::new),
                        inputs,
                    }
                    .into())
                } else {
                    let operators = VecDeque::from(operators);

                    let (mut max_index, mut max_expr, mut max_syntax, mut max_precedence) =
                        operators.front().cloned().unwrap();

                    for (index, expr, syntax, precedence) in operators.iter().skip(1).cloned() {
                        macro_rules! replace {
                            () => {{
                                max_index = index;
                                max_expr = expr;
                                max_syntax = syntax;
                                max_precedence = precedence;
                            }};
                        }

                        match precedence.cmp(&max_precedence) {
                            Ordering::Greater => replace!(),
                            Ordering::Less => continue,
                            Ordering::Equal => match precedence.associativity() {
                                OperatorAssociativity::Left => {
                                    if index > max_index {
                                        replace!();
                                    }
                                }
                                OperatorAssociativity::Right => {
                                    if index < max_index {
                                        replace!()
                                    }
                                }
                                OperatorAssociativity::None => {
                                    self.ast_builder.driver.syntax_error_with([
                                        (
                                            exprs[index].span,
                                            String::from("only one of this operator may be provided at a time"),
                                        ),
                                        (
                                            exprs[max_index].span,
                                            String::from("first use of this operator"),
                                        ),
                                    ]);

                                    return Err(self.ast_builder.syntax_error(list_span));
                                }
                            },
                        }
                    }

                    let rhs = exprs.split_off(max_index + 1);
                    let mut lhs = exprs;
                    let operator = lhs.pop().unwrap();

                    if rhs.is_empty() {
                        self.ast_builder.driver.syntax_error(
                            max_expr.span,
                            "expected values on right side of operator",
                        );

                        Err(self.ast_builder.syntax_error(list_span))
                    } else if lhs.is_empty() {
                        self.ast_builder.driver.syntax_error(
                            max_expr.span,
                            "expected values on left side of operator",
                        );

                        Err(self.ast_builder.syntax_error(list_span))
                    } else {
                        let lhs_span =
                            Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span);

                        let lhs = parse::Expr {
                            span: lhs_span,
                            kind: parse::ExprKind::List(vec![lhs.into()]),
                        };

                        let rhs_span =
                            Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span);

                        let rhs = parse::Expr {
                            span: rhs_span,
                            kind: parse::ExprKind::List(vec![rhs.into()]),
                        };

                        let list_span = Span::join(lhs_span, rhs_span);

                        self.expand_syntax(list_span, max_syntax, vec![lhs, operator, rhs], scope)
                            .await
                    }
                }
            }
        }
    }

    fn operators_in_list<'a>(
        &'a self,
        exprs: impl IntoIterator<Item = (usize, &'a parse::Expr<D>)>,
        scope: D::Scope,
    ) -> Vec<(
        usize,
        parse::Expr<D>,
        SyntaxAssignmentValue<D>,
        OperatorPrecedenceStatementAttributeKind,
    )> {
        exprs
            .into_iter()
            .filter_map(move |(index, expr)| {
                if let parse::ExprKind::Name(name, name_scope) = &expr.kind {
                    let syntax = self.ast_builder.file.resolve_syntax(
                        expr.span,
                        name.clone(),
                        name_scope.unwrap_or(scope),
                    );

                    if let Some(syntax) = syntax {
                        if let Some(attribute) = &syntax.operator_precedence {
                            let precedence = attribute.precedence;

                            return Some((index, expr.clone(), syntax, precedence));
                        }
                    }
                }

                None
            })
            .collect()
    }

    async fn expand_syntax(
        &self,
        span: D::Span,
        syntax: SyntaxAssignmentValue<D>,
        exprs: Vec<parse::Expr<D>>,
        scope: D::Scope,
    ) -> Result<Expression<D>, SyntaxError<D>> {
        let SyntaxBody::Block(body) = syntax.body?;

        for rule in body.rules.into_iter().flatten() {
            let SyntaxRule::<D>::Function(rule) = rule;

            let pattern = rule.pattern.into_iter().collect::<Result<Vec<_>, _>>();

            let (pattern, body) = match (pattern, rule.body) {
                (Ok(pattern), Ok(body)) => (pattern, *body),
                _ => continue,
            };

            let vars = match SyntaxPattern::r#match(&self.ast_builder, pattern, &exprs) {
                Some(vars) => vars,
                None => continue,
            };

            let body = SyntaxPattern::expand(&self.ast_builder, body, &vars, span, scope)?;

            return self
                .ast_builder
                .build_expr::<ExpressionSyntax>(self.clone(), body, scope)
                .await;
        }

        self.ast_builder.driver.syntax_error_with([
            (
                span,
                String::from("expected values on right side of operator"),
            ),
            (syntax.syntax_span, String::from("syntax defined here")),
        ]);

        Err(self.ast_builder.syntax_error(span))
    }
}
