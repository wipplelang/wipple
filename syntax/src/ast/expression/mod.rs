definitions! {
    mod annotate;
    mod end;
    mod external;
    mod function;
    mod intrinsic;
    mod plugin;
    mod semantics;
    mod tuple;
    mod when;
    mod r#where;
    mod with;
}

use crate::ScopeSet;
use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, OperatorAssociativity, OperatorPrecedenceStatementAttributeKind, Statement,
        StatementAttributes, StatementSyntax, SyntaxAssignmentValue, SyntaxBody, SyntaxPattern,
        SyntaxRule,
    },
    parse, Driver, File, Fix, FixRange, ResolveSyntaxError, Span,
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
            End,
            External,
            Intrinsic,
            Plugin,
            Semantics,
            With,
            Where,
            When,
        },
        terminal: {
            Unit,
            Name,
            Text,
            Number,
            Asset,
            Call,
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitExpression<D: Driver> {
    pub span: D::Span,
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
    pub scope_set: ScopeSet<D::Scope>,
}

impl<D: Driver> NameExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.name.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct TextExpression<D: Driver> {
    pub span: D::Span,
    pub text: parse::Text<D>,
}

impl<D: Driver> TextExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TextExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("\"{}\"", self.text.raw().as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct NumberExpression<D: Driver> {
    pub span: D::Span,
    pub number: D::InternedString,
}

impl<D: Driver> NumberExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NumberExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.number.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct AssetExpression<D: Driver> {
    pub span: D::Span,
    pub raw: D::InternedString,
}

impl<D: Driver> AssetExpression<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for AssetExpression<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("`{}`", self.raw.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression<D: Driver> {
    pub span: D::Span,
    pub function: Result<Box<Expression<D>>, SyntaxError<D>>,
    pub inputs: Vec<Result<Expression<D>, SyntaxError<D>>>,
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
    const BLOCK_CREATES_SCOPE: bool = true;

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
        Ok(BlockExpression {
            span,
            statements: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => self.expand_list(span, exprs.collect(), scope_set).await,
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, name_scope) => Ok(NameExpression {
                    span: expr.span,
                    name,
                    scope_set: name_scope.unwrap_or_else(|| scope_set.lock().clone()),
                }
                .into()),
                parse::ExprKind::Text(text) => Ok(TextExpression {
                    span: expr.span,
                    text,
                }
                .into()),
                parse::ExprKind::Number(number) => Ok(NumberExpression {
                    span: expr.span,
                    number,
                }
                .into()),
                parse::ExprKind::Asset(raw) => Ok(AssetExpression {
                    span: expr.span,
                    raw,
                }
                .into()),
                parse::ExprKind::Block(_) => {
                    self.ast_builder
                        .build_expr::<ExpressionSyntax>(self.clone(), expr, scope_set)
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
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Expression<D>, SyntaxError<D>> {
        match exprs.len() {
            0 => Ok(UnitExpression { span: list_span }.into()),
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name, name_scope) = &expr.kind {
                    let syntax = self.ast_builder.file.resolve_syntax(
                        expr.span,
                        name.clone(),
                        name_scope
                            .clone()
                            .unwrap_or_else(|| scope_set.lock().clone()),
                    );

                    match syntax {
                        Ok(syntax) => {
                            return self
                                .expand_syntax(expr.span, syntax, vec![expr], scope_set)
                                .await
                        }
                        Err(ResolveSyntaxError::NotFound) => {}
                        Err(ResolveSyntaxError::Ambiguous) => {
                            self.ast_builder.driver.syntax_error(
                                expr.span,
                                format!("`{}` is ambiguous in this context", name.as_ref()),
                            );

                            return Err(self.ast_builder.syntax_error(list_span));
                        }
                    }
                }

                self.clone().build_terminal(expr, scope_set).await
            }
            _ => {
                let operators = self.operators_in_list(exprs.iter().enumerate(), scope_set.clone());

                if operators.is_empty() {
                    for expr in &exprs {
                        if let parse::ExprKind::Name(name, name_scope) = &expr.kind {
                            let syntax = self.ast_builder.file.resolve_syntax(
                                expr.span,
                                name.clone(),
                                name_scope
                                    .clone()
                                    .unwrap_or_else(|| scope_set.lock().clone()),
                            );

                            match syntax {
                                Ok(syntax) => {
                                    return self
                                        .expand_syntax(expr.span, syntax, exprs, scope_set)
                                        .await
                                }
                                Err(ResolveSyntaxError::NotFound) => {}
                                Err(ResolveSyntaxError::Ambiguous) => {
                                    self.ast_builder.driver.syntax_error(
                                        expr.span,
                                        format!("`{}` is ambiguous in this context", name.as_ref()),
                                    );

                                    return Err(self.ast_builder.syntax_error(list_span));
                                }
                            }
                        }
                    }

                    let mut exprs = exprs.into_iter();
                    let first = exprs.next().unwrap();

                    let function = self
                        .ast_builder
                        .build_expr::<ExpressionSyntax>(self.clone(), first, scope_set.clone())
                        .await;

                    let inputs = stream::iter(exprs)
                        .then(|expr| {
                            self.ast_builder.build_expr::<ExpressionSyntax>(
                                self.clone(),
                                expr,
                                scope_set.clone(),
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
                                    self.ast_builder.driver.syntax_error_with(
                                        [
                                            (
                                                exprs[index].span,
                                                String::from("only one of this operator may be provided at a time"),
                                            ),
                                            (
                                                exprs[max_index].span,
                                                String::from("first use of this operator"),
                                            ),
                                        ],
                                        Some(Fix::new("remove this operator", FixRange::replace(exprs[index].span), "")),
                                    );

                                    return Err(self.ast_builder.syntax_error(list_span));
                                }
                            },
                        }
                    }

                    let rhs = exprs.split_off(max_index + 1);
                    let mut lhs = exprs;
                    let operator = lhs.pop().unwrap();

                    let lhs_count = lhs.len();

                    let lhs_span = lhs
                        .first()
                        .zip(lhs.last())
                        .map(|(first, last)| Span::join(first.span, last.span));

                    let lhs = lhs_span.map(|span| parse::Expr::list_or_expr(span, lhs));

                    let rhs_count = rhs.len();

                    let rhs_span = rhs
                        .first()
                        .zip(rhs.last())
                        .map(|(first, last)| Span::join(first.span, last.span));

                    let rhs = rhs_span.map(|span| parse::Expr::list_or_expr(span, rhs));

                    let mut list_span = Span::join(
                        lhs_span.unwrap_or(operator.span),
                        rhs_span.unwrap_or(operator.span),
                    );

                    list_span.set_caller(max_expr.span);

                    if lhs_count > 1 || rhs_count > 1 {
                        list_span.set_expanded_from_operator();
                    }

                    let input = lhs
                        .into_iter()
                        .chain(std::iter::once(operator))
                        .chain(rhs)
                        .collect();

                    self.expand_syntax(list_span, max_syntax, input, scope_set)
                        .await
                }
            }
        }
    }

    fn operators_in_list<'a>(
        &'a self,
        exprs: impl IntoIterator<Item = (usize, &'a parse::Expr<D>)>,
        scope_set: Shared<ScopeSet<D::Scope>>,
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
                        name_scope
                            .clone()
                            .unwrap_or_else(|| scope_set.lock().clone()),
                    );

                    match syntax {
                        Ok(syntax) => {
                            if let Some(attribute) = &syntax.operator_precedence {
                                let precedence = attribute.precedence;

                                return Some((index, expr.clone(), syntax, precedence));
                            }
                        }
                        Err(ResolveSyntaxError::NotFound) => {}
                        Err(ResolveSyntaxError::Ambiguous) => {
                            self.ast_builder.driver.syntax_error(
                                expr.span,
                                format!("`{}` is ambiguous in this context", name.as_ref()),
                            );
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
        mut exprs: Vec<parse::Expr<D>>,
        scope_set: Shared<ScopeSet<D::Scope>>,
    ) -> Result<Expression<D>, SyntaxError<D>> {
        for expr in &mut exprs {
            // The empty set is a marker that we can detect below to restore
            // the scopes after expansion
            expr.fix_to(&ScopeSet::new());
        }

        let SyntaxBody::Block(body) = syntax.body?;
        let mut body_scope_set = body.scope_set;
        body_scope_set.insert(self.ast_builder.file.make_scope());

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

            let mut body =
                SyntaxPattern::expand(&self.ast_builder, body, &vars, span, &body_scope_set)?;

            // Restore the scopes after expansion
            body.remove_empty();

            return self
                .ast_builder
                .build_expr::<ExpressionSyntax>(self.clone(), body, scope_set)
                .await;
        }

        self.ast_builder.driver.syntax_error_with(
            [
                (span, String::from("syntax did not match any rules")),
                (syntax.syntax_span, String::from("syntax defined here")),
            ],
            None,
        );

        Err(self.ast_builder.syntax_error(span))
    }
}
