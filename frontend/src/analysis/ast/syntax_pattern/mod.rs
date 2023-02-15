use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, StatementAttributes,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
    ScopeId,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use std::collections::{HashMap, HashSet};

syntax_group! {
    #[derive(Debug, Clone)]
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
pub struct UnitSyntaxPattern {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NameSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
    pub scope: ScopeId,
}

#[derive(Debug, Clone)]
pub struct TextSyntaxPattern {
    pub span: Span,
    pub text: InternedString,
}

#[derive(Debug, Clone)]
pub struct NumberSyntaxPattern {
    pub span: Span,
    pub number: InternedString,
}

#[derive(Debug, Clone)]
pub struct UnderscoreSyntaxPattern {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariableSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct VariableRepetitionSyntaxPattern {
    pub span: Span,
    pub name: InternedString,
}

#[derive(Debug, Clone)]
pub struct ListSyntaxPattern {
    pub span: Span,
    pub patterns: Vec<Result<SyntaxPattern, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct ListRepetitionSyntaxPattern {
    pub span: Span,
    pub patterns: Vec<Result<SyntaxPattern, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct BlockSyntaxPattern {
    pub span: Span,
    pub statements: Vec<Result<SyntaxPattern, SyntaxError>>,
}

#[derive(Clone)]
pub struct SyntaxPatternSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for SyntaxPatternSyntaxContext {
    type Body = SyntaxPattern;
    type Statement = SyntaxPatternSyntax;

    fn new(ast_builder: AstBuilder) -> Self {
        SyntaxPatternSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
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
        _scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Ok(BlockSyntaxPattern {
            span,
            statements: statements.collect(),
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
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
                    parse::ExprKind::Text(text) => Ok(TextSyntaxPattern {
                        span: expr.span,
                        text,
                    }
                    .into()),
                    parse::ExprKind::Number(number) => Ok(NumberSyntaxPattern {
                        span: expr.span,
                        number,
                    }
                    .into()),
                    _ => {
                        self.ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(expr.span, "expected syntax pattern")],
                        );

                        Err(self.ast_builder.syntax_error(expr.span))
                    }
                },
            },
        }
    }
}

#[derive(Debug, Clone)]
pub(in crate::analysis::ast) enum SyntaxExpression {
    Single(parse::Expr),
    Repeated(Vec<parse::Expr>),
}

impl SyntaxPattern {
    pub(in crate::analysis::ast) fn r#match<'a>(
        ast_builder: &AstBuilder,
        rule: impl IntoIterator<Item = SyntaxPattern>,
        input: impl IntoIterator<Item = &'a parse::Expr> + Clone,
    ) -> Option<HashMap<InternedString, SyntaxExpression>> {
        let mut vars = HashMap::new();
        Self::match_inner(ast_builder, rule.into_iter(), input.into_iter(), &mut vars)?;
        Some(vars)
    }

    fn match_inner<'a>(
        ast_builder: &AstBuilder,
        rule: impl Iterator<Item = SyntaxPattern>,
        mut input: impl Iterator<Item = &'a parse::Expr>,
        vars: &mut HashMap<InternedString, SyntaxExpression>,
    ) -> Option<()> {
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

                    if let parse::ExprKind::Name(name, _) = expr.kind {
                        if name == pattern.name {
                            continue;
                        }
                    }

                    return None;
                }
                SyntaxPattern::Number(pattern) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::Number(number) = expr.kind {
                        if number == pattern.number {
                            continue;
                        }
                    }

                    return None;
                }

                SyntaxPattern::Text(pattern) => {
                    let expr = input.next()?;

                    if let parse::ExprKind::Text(text) = expr.kind {
                        if text == pattern.text {
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
                    ast_builder.compiler.add_error(
                        "list repetitions may not appear inside a syntax pattern",
                        vec![Note::primary(pattern.span, "try removing this")],
                    );

                    return None;
                }
                SyntaxPattern::Block(pattern) => {
                    ast_builder.compiler.add_error(
                        "blocks in syntax patterns are not yet supported",
                        vec![Note::primary(pattern.span, "try removing this")],
                    );

                    return None;
                }
            }
        }

        Some(())
    }

    pub(in crate::analysis::ast) fn expand(
        ast_builder: &AstBuilder,
        body: SyntaxPattern,
        vars: &HashMap<InternedString, SyntaxExpression>,
    ) -> Result<parse::Expr, SyntaxError> {
        // TODO: Have a `span()` function instead of this
        let body_span = match &body {
            SyntaxPattern::Unit(pattern) => pattern.span,
            SyntaxPattern::Name(pattern) => pattern.span,
            SyntaxPattern::Text(pattern) => pattern.span,
            SyntaxPattern::Number(pattern) => pattern.span,
            SyntaxPattern::Underscore(pattern) => pattern.span,
            SyntaxPattern::Variable(pattern) => pattern.span,
            SyntaxPattern::VariableRepetition(pattern) => pattern.span,
            SyntaxPattern::List(pattern) => pattern.span,
            SyntaxPattern::ListRepetition(pattern) => pattern.span,
            SyntaxPattern::Block(pattern) => pattern.span,
        };

        let mut exprs = Self::expand_inner(ast_builder, body, vars)?;

        (exprs.len() == 1)
            .then(|| exprs.pop().unwrap())
            .ok_or_else(|| {
                ast_builder.compiler.add_error(
                    "syntax body must produce a single expression",
                    vec![Note::primary(
                        body_span,
                        "try wrapping this in `()` or `{}`",
                    )],
                );

                ast_builder.syntax_error(body_span)
            })
    }

    fn expand_inner(
        ast_builder: &AstBuilder,
        body: SyntaxPattern,
        vars: &HashMap<InternedString, SyntaxExpression>,
    ) -> Result<Vec<parse::Expr>, SyntaxError> {
        Ok(match body {
            SyntaxPattern::Unit(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::List(Vec::new()),
            }],
            SyntaxPattern::Name(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Name(pattern.name, Some(pattern.scope)),
            }],
            SyntaxPattern::Text(pattern) => vec![parse::Expr {
                span: pattern.span,
                kind: parse::ExprKind::Text(pattern.text),
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
                        ast_builder.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                pattern.span,
                                "expansion of repeated variable must occur inside `...()`",
                            )],
                        );

                        return Err(ast_builder.syntax_error(pattern.span));
                    }
                },
                None => {
                    ast_builder.compiler.add_error(
                        format!("cannot find `{}`", pattern.name),
                        vec![Note::primary(
                            pattern.span,
                            "this syntax variable is not defined",
                        )],
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
                    ast_builder.compiler.add_error(
                        format!("cannot find `{}`", pattern.name),
                        vec![Note::primary(
                            pattern.span,
                            "this syntax variable is not defined",
                        )],
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
                        .map(|pattern| Self::expand_inner(ast_builder, pattern?, vars))
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
                            ast_builder.compiler.add_error(
                                format!("cannot find `{var}`"),
                                vec![Note::primary(
                                    pattern.span,
                                    "this syntax variable is not defined",
                                )],
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
                        ast_builder.compiler.add_error(
                            "list repetition must contain a repeated variable",
                            vec![Note::primary(
                                pattern.span,
                                "try removing the `...` from this",
                            )],
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
                                        repeated_var,
                                        SyntaxExpression::Single(value.clone()),
                                    )))
                                    .collect();

                                Ok(pattern
                                    .patterns
                                    .clone()
                                    .into_iter()
                                    .map(|pattern| Self::expand_inner(ast_builder, pattern?, &vars))
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
                        let mut vars = used_repetitions
                            .into_keys()
                            .map(|var| format!("`{var}`"))
                            .collect::<Vec<_>>();

                        let last = vars.pop().unwrap();

                        ast_builder.compiler.add_error(
                            "list repetition may not contain multiple repeated variables",
                            vec![Note::primary(
                                pattern.span,
                                format!("try removing {} or {}", vars.join(", "), last),
                            )],
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
                                let statement = Self::expand(ast_builder, pattern?, vars)?;

                                Ok(parse::Statement {
                                    lines: vec![parse::ListLine {
                                        exprs: match statement.try_into_list_exprs() {
                                            Ok((_, exprs)) => exprs.collect(),
                                            Err(statement) => vec![statement],
                                        },
                                        ..Default::default()
                                    }],
                                    ..Default::default()
                                })
                            })
                            .collect::<Result<_, _>>()?,
                    ),
                }]
            }
        })
    }

    fn collect_used_variables(&self, vars: &mut HashSet<InternedString>) {
        match self {
            SyntaxPattern::Unit(_)
            | SyntaxPattern::Name(_)
            | SyntaxPattern::Number(_)
            | SyntaxPattern::Text(_)
            | SyntaxPattern::Underscore(_)
            | SyntaxPattern::ListRepetition(_) => {}
            SyntaxPattern::Variable(pattern) => {
                vars.insert(pattern.name);
            }
            SyntaxPattern::VariableRepetition(pattern) => {
                vars.insert(pattern.name);
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
