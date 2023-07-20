definitions! {}

use crate::{
    ast::{
        format::Format,
        macros::{definitions, syntax_group},
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    parse, Driver, Span,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use std::collections::{HashMap, HashSet};
use wipple_util::Shared;

syntax_group! {
    pub type SyntaxPattern<SyntaxPatternSyntaxContext> {
        non_terminal: {},
        terminal: {
            Unit,
            Name,
            Text,
            Number,
            Underscore,
            Variable,
            VariableRepetition,
            List,
            ListRepetition,
            Block,
        },
    }
}

#[derive(Debug, Clone)]
pub struct UnitSyntaxPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnitSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnitSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("()"))
    }
}

#[derive(Debug, Clone)]
pub struct NameSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
    pub scope: D::Scope,
}

impl<D: Driver> NameSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NameSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.name.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct TextSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub text: D::InternedString,
    pub raw: D::InternedString,
}
impl<D: Driver> TextSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for TextSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("\"{}\"", self.raw.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct NumberSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub number: D::InternedString,
}

impl<D: Driver> NumberSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for NumberSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(self.number.as_ref().to_string())
    }
}

#[derive(Debug, Clone)]
pub struct UnderscoreSyntaxPattern<D: Driver> {
    pub span: D::Span,
}

impl<D: Driver> UnderscoreSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for UnderscoreSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(String::from("_"))
    }
}

#[derive(Debug, Clone)]
pub struct VariableSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
}

impl<D: Driver> VariableSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for VariableSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("'{}", self.name.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct VariableRepetitionSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub name: D::InternedString,
}

impl<D: Driver> VariableRepetitionSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for VariableRepetitionSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!("...{}", self.name.as_ref()))
    }
}

#[derive(Debug, Clone)]
pub struct ListSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub patterns: Vec<Result<SyntaxPattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> ListSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ListSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "({})",
            self.patterns
                .into_iter()
                .map(|pattern| pattern?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct ListRepetitionSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub patterns: Vec<Result<SyntaxPattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> ListRepetitionSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

impl<D: Driver> Format<D> for ListRepetitionSyntaxPattern<D> {
    fn format(self) -> Result<String, SyntaxError<D>> {
        Ok(format!(
            "...({})",
            self.patterns
                .into_iter()
                .map(|pattern| pattern?.format())
                .collect::<Result<Vec<_>, _>>()?
                .join(" ")
        ))
    }
}

#[derive(Debug, Clone)]
pub struct BlockSyntaxPattern<D: Driver> {
    pub span: D::Span,
    pub statements: Vec<Result<SyntaxPattern<D>, SyntaxError<D>>>,
}

impl<D: Driver> Format<D> for BlockSyntaxPattern<D> {
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

impl<D: Driver> BlockSyntaxPattern<D> {
    pub fn span(&self) -> D::Span {
        self.span
    }
}

#[derive(Clone)]
pub struct SyntaxPatternSyntaxContext<D: Driver> {
    pub(super) ast_builder: AstBuilder<D>,
    statement_attributes: Option<Shared<StatementAttributes<D>>>,
}

#[async_trait]
impl<D: Driver> SyntaxContext<D> for SyntaxPatternSyntaxContext<D> {
    type Body = SyntaxPattern<D>;
    type Statement = SyntaxPatternSyntax;

    fn new(ast_builder: AstBuilder<D>) -> Self {
        SyntaxPatternSyntaxContext {
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
        _scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        Ok(BlockSyntaxPattern {
            span,
            statements: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr<D>,
        scope: D::Scope,
    ) -> Result<Self::Body, SyntaxError<D>> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => {
                let patterns = stream::iter(exprs)
                    .then(|expr| {
                        self.ast_builder.build_expr::<SyntaxPatternSyntax>(
                            self.clone(),
                            expr,
                            scope,
                        )
                    })
                    .collect::<Vec<_>>()
                    .await;

                Ok(ListSyntaxPattern { span, patterns }.into())
            }
            Err(expr) => match expr.try_into_list_repetition_exprs() {
                Ok((span, exprs)) => {
                    let patterns = stream::iter(exprs)
                        .then(|expr| {
                            self.ast_builder.build_expr::<SyntaxPatternSyntax>(
                                self.clone(),
                                expr,
                                scope,
                            )
                        })
                        .collect::<Vec<_>>()
                        .await;

                    Ok(ListRepetitionSyntaxPattern { span, patterns }.into())
                }
                Err(expr) => match expr.kind {
                    parse::ExprKind::Underscore => {
                        Ok(UnderscoreSyntaxPattern { span: expr.span }.into())
                    }
                    parse::ExprKind::Name(name, name_scope) => Ok(NameSyntaxPattern {
                        span: expr.span,
                        name,
                        scope: name_scope.unwrap_or(scope),
                    }
                    .into()),
                    parse::ExprKind::QuoteName(name) => Ok(VariableSyntaxPattern {
                        span: expr.span,
                        name,
                    }
                    .into()),
                    parse::ExprKind::RepeatName(name) => Ok(VariableRepetitionSyntaxPattern {
                        span: expr.span,
                        name,
                    }
                    .into()),
                    parse::ExprKind::Text(text, raw) => Ok(TextSyntaxPattern {
                        span: expr.span,
                        text,
                        raw,
                    }
                    .into()),
                    parse::ExprKind::Number(number) => Ok(NumberSyntaxPattern {
                        span: expr.span,
                        number,
                    }
                    .into()),
                    _ => {
                        self.ast_builder
                            .driver
                            .syntax_error(expr.span, "expected syntax pattern");

                        Err(self.ast_builder.syntax_error(expr.span))
                    }
                },
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum SyntaxExpression<D: Driver> {
    Single(parse::Expr<D>),
    Repeated(Vec<parse::Expr<D>>),
}

impl<D: Driver> SyntaxPattern<D> {
    pub(crate) fn r#match<'a>(
        ast_builder: &AstBuilder<D>,
        rule: impl IntoIterator<Item = SyntaxPattern<D>>,
        input: impl IntoIterator<Item = &'a parse::Expr<D>> + Clone,
    ) -> Option<HashMap<D::InternedString, SyntaxExpression<D>>>
    where
        D: 'a,
    {
        let mut vars = HashMap::new();
        Self::match_inner(ast_builder, rule.into_iter(), input.into_iter(), &mut vars)?;
        Some(vars)
    }

    fn match_inner<'a>(
        ast_builder: &AstBuilder<D>,
        rule: impl Iterator<Item = SyntaxPattern<D>>,
        mut input: impl Iterator<Item = &'a parse::Expr<D>>,
        vars: &mut HashMap<D::InternedString, SyntaxExpression<D>>,
    ) -> Option<()>
    where
        D: 'a,
    {
        let mut rule = rule.peekable();

        for pattern in &mut rule {
            match pattern {
                SyntaxPattern::Underscore(_) => {
                    input.next()?;
                }
                SyntaxPattern::Unit(_) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::List(list) = &expr.kind {
                        if list.is_empty() {
                            continue;
                        }
                    }

                    return None;
                }
                SyntaxPattern::Name(pattern) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::Name(name, _) = &expr.kind {
                        if *name == pattern.name {
                            continue;
                        }
                    }

                    return None;
                }
                SyntaxPattern::Number(pattern) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::Number(number) = &expr.kind {
                        if *number == pattern.number {
                            continue;
                        }
                    }

                    return None;
                }

                SyntaxPattern::Text(pattern) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::Text(text, _) = &expr.kind {
                        if *text == pattern.text {
                            continue;
                        }
                    }

                    return None;
                }
                SyntaxPattern::Variable(pattern) => {
                    let expr = input.next()?;

                    vars.insert(pattern.name, SyntaxExpression::Single(expr.clone()));
                }
                SyntaxPattern::VariableRepetition(pattern) => {
                    if rule.peek().is_some() {
                        // Variable repetition captures all remaining inputs, so if
                        // there are more patterns after this, the rule must fail
                        return None;
                    }

                    vars.insert(
                        pattern.name,
                        SyntaxExpression::Repeated(input.cloned().collect()),
                    );

                    break; // we verified that this is the last rule above
                }
                SyntaxPattern::List(pattern) => {
                    let expr = input.next()?;

                    let (_, list) = match expr.try_as_list_exprs() {
                        Ok(list) => list,
                        _ => return None,
                    };

                    Self::match_inner(
                        ast_builder,
                        pattern.patterns.into_iter().flatten(),
                        list,
                        vars,
                    )?;
                }
                SyntaxPattern::ListRepetition(pattern) => {
                    ast_builder.driver.syntax_error(
                        pattern.span,
                        "list repetitions may not appear inside a syntax pattern",
                    );

                    return None;
                }
                SyntaxPattern::Block(pattern) => {
                    ast_builder.driver.syntax_error(
                        pattern.span,
                        "blocks in syntax patterns are not yet supported",
                    );

                    return None;
                }
            }
        }

        Some(())
    }

    pub(crate) fn expand(
        ast_builder: &AstBuilder<D>,
        body: SyntaxPattern<D>,
        vars: &HashMap<D::InternedString, SyntaxExpression<D>>,
        source_span: D::Span,
        scope: D::Scope,
    ) -> Result<parse::Expr<D>, SyntaxError<D>> {
        let body_span = body.span();

        let mut exprs = Self::expand_inner(ast_builder, body, vars, source_span, scope)?;

        (exprs.len() == 1)
            .then(|| exprs.pop().unwrap())
            .ok_or_else(|| {
                ast_builder.driver.syntax_error_with(
                    [
                        (
                            body_span,
                            String::from("syntax body must produce a single expression"),
                        ),
                        (body_span, String::from("try wrapping this in `()` or `{}`")),
                    ],
                    None,
                );

                ast_builder.syntax_error(body_span)
            })
    }

    fn expand_inner(
        ast_builder: &AstBuilder<D>,
        body: SyntaxPattern<D>,
        vars: &HashMap<D::InternedString, SyntaxExpression<D>>,
        source_span: D::Span,
        scope: D::Scope,
    ) -> Result<Vec<parse::Expr<D>>, SyntaxError<D>> {
        let mut result = match body {
            SyntaxPattern::Unit(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::List(Vec::new()),
            }],
            SyntaxPattern::Name(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Name(pattern.name, Some(scope)),
            }],
            SyntaxPattern::Text(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Text(pattern.text.clone(), pattern.text),
            }],
            SyntaxPattern::Number(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Number(pattern.number),
            }],
            SyntaxPattern::Underscore(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Underscore,
            }],
            SyntaxPattern::Variable(pattern) => match vars.get(&pattern.name) {
                Some(expr) => match expr {
                    SyntaxExpression::Single(expr) => vec![expr.clone()],
                    SyntaxExpression::Repeated(_) => {
                        ast_builder.driver.syntax_error(
                            pattern.span,
                            "expansion of repeated variable must occur inside `...()`",
                        );

                        return Err(ast_builder.syntax_error(pattern.span));
                    }
                },
                None => {
                    ast_builder.driver.syntax_error(
                        pattern.span,
                        format!("cannot find syntax variable `{}`", pattern.name.as_ref()),
                    );

                    return Err(ast_builder.syntax_error(pattern.span));
                }
            },
            SyntaxPattern::VariableRepetition(pattern) => match vars.get(&pattern.name) {
                Some(expr) => match expr {
                    SyntaxExpression::Single(expr) => vec![expr.clone()],
                    SyntaxExpression::Repeated(exprs) => exprs.clone(),
                },
                None => {
                    ast_builder.driver.syntax_error(
                        pattern.span,
                        format!("cannot find syntax variable `{}`", pattern.name.as_ref()),
                    );

                    return Err(ast_builder.syntax_error(pattern.span));
                }
            },
            SyntaxPattern::List(pattern) => {
                vec![parse::Expr::list_or_expr(
                    pattern.span,
                    pattern
                        .patterns
                        .into_iter()
                        .map(|pattern| {
                            Self::expand_inner(ast_builder, pattern?, vars, source_span, scope)
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .flatten()
                        .collect(),
                )]
            }
            SyntaxPattern::ListRepetition(pattern) => {
                let mut used_vars = HashSet::new();
                for pattern in pattern.patterns.iter().flatten() {
                    pattern.collect_used_variables(&mut used_vars);
                }

                let used_repetitions = used_vars
                    .into_iter()
                    .map(|var| match vars.get(&var) {
                        Some(expr) => Ok(match expr {
                            SyntaxExpression::Single(_) => None,
                            SyntaxExpression::Repeated(exprs) => Some((var, exprs)),
                        }),
                        None => {
                            ast_builder.driver.syntax_error(
                                pattern.span,
                                format!("cannot find syntax variable `{}`", var.as_ref()),
                            );

                            Err(ast_builder.syntax_error(pattern.span))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect::<HashMap<_, _>>();

                match used_repetitions.len() {
                    0 => {
                        ast_builder.driver.syntax_error(
                            pattern.span,
                            "list repetition must contain a repeated variable",
                        );

                        return Err(ast_builder.syntax_error(pattern.span));
                    }
                    1 => {
                        let (repeated_var, values) = used_repetitions.into_iter().next().unwrap();

                        values
                            .iter()
                            .map(|value| {
                                let vars = vars
                                    .clone()
                                    .into_iter()
                                    .chain(std::iter::once((
                                        repeated_var.clone(),
                                        SyntaxExpression::Single(value.clone()),
                                    )))
                                    .collect();

                                Ok(pattern
                                    .patterns
                                    .clone()
                                    .into_iter()
                                    .map(|pattern| {
                                        Self::expand_inner(
                                            ast_builder,
                                            pattern?,
                                            &vars,
                                            source_span,
                                            scope,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?
                                    .into_iter()
                                    .flatten())
                            })
                            .collect::<Result<Vec<_>, _>>()?
                            .into_iter()
                            .flatten()
                            .collect()
                    }
                    _ => {
                        ast_builder.driver.syntax_error(
                            pattern.span,
                            "list repetition may not contain multiple repeated variables",
                        );

                        return Err(ast_builder.syntax_error(pattern.span));
                    }
                }
            }
            SyntaxPattern::Block(pattern) => {
                vec![parse::Expr {
                    span: pattern.span,
                    kind: parse::ExprKind::Block(
                        pattern
                            .statements
                            .into_iter()
                            .map(|pattern| {
                                let statement =
                                    Self::expand(ast_builder, pattern?, vars, source_span, scope)?;

                                Ok(parse::Statement {
                                    lines: vec![parse::ListLine {
                                        leading_lines: 0,
                                        attributes: Vec::new(),
                                        exprs: match statement.try_into_list_exprs() {
                                            Ok((_, exprs)) => exprs.collect(),
                                            Err(statement) => vec![statement],
                                        },
                                        comment: None,
                                    }],
                                })
                            })
                            .collect::<Result<_, _>>()?,
                    ),
                }]
            }
        };

        for expr in &mut result {
            expr.span.merge(source_span);
        }

        Ok(result)
    }

    fn collect_used_variables(&self, vars: &mut HashSet<D::InternedString>) {
        match self {
            SyntaxPattern::Unit(_)
            | SyntaxPattern::Name(_)
            | SyntaxPattern::Number(_)
            | SyntaxPattern::Text(_)
            | SyntaxPattern::Underscore(_)
            | SyntaxPattern::ListRepetition(_) => {}
            SyntaxPattern::Variable(pattern) => {
                vars.insert(pattern.name.clone());
            }
            SyntaxPattern::VariableRepetition(pattern) => {
                vars.insert(pattern.name.clone());
            }
            SyntaxPattern::List(pattern) => {
                for pattern in pattern.patterns.iter().flatten() {
                    pattern.collect_used_variables(vars);
                }
            }
            SyntaxPattern::Block(pattern) => {
                for pattern in pattern.statements.iter().flatten() {
                    pattern.collect_used_variables(vars);
                }
            }
        }
    }
}
