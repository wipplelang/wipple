mod annotate;
mod end;
mod external;
mod format;
mod function;
mod tuple;
mod when;

pub use annotate::AnnotateExpression;
pub use end::EndExpression;
pub use external::ExternalExpression;
pub use format::FormatExpression;
pub use function::FunctionExpression;
pub use tuple::TupleExpression;
pub use when::WhenExpression;

use annotate::*;
use end::*;
use external::*;
use format::*;
use function::*;
use tuple::*;
use when::*;

use crate::{
    analysis::ast::{
        syntax::{Syntax, SyntaxContext, SyntaxError},
        AstBuilder, OperatorAssociativity, OperatorPrecedenceStatementAttributeKind, Statement,
        StatementAttributes, StatementSyntax, SyntaxAssignmentValue, SyntaxBody, SyntaxPattern,
        SyntaxRule,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, SpanList},
    ScopeId,
};
use async_trait::async_trait;
use futures::{stream, StreamExt};
use std::{cmp::Ordering, collections::VecDeque};

syntax_group! {
    #[derive(Debug, Clone)]
    pub type Expression<ExpressionSyntaxContext> {
        non_terminal: {
            Function,
            Tuple,
            Annotate,
            End,
            External,
            Format,
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
pub struct UnitExpression {
    pub span: SpanList,
}

impl UnitExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct NameExpression {
    pub span: SpanList,
    pub name: InternedString,
    pub scope: ScopeId,
}

impl NameExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct TextExpression {
    pub span: SpanList,
    pub text: InternedString,
}

impl TextExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct NumberExpression {
    pub span: SpanList,
    pub number: InternedString,
}

impl NumberExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub span: SpanList,
    pub function: Result<Box<Expression>, SyntaxError>,
    pub inputs: Vec<Result<Expression, SyntaxError>>,
}

impl CallExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub span: SpanList,
    pub statements: Vec<Result<Statement, SyntaxError>>,
    pub scope: ScopeId,
}

impl BlockExpression {
    pub fn span(&self) -> SpanList {
        self.span
    }
}

#[derive(Clone)]
pub struct ExpressionSyntaxContext {
    pub(super) ast_builder: AstBuilder,
    statement_attributes: Option<Shared<StatementAttributes>>,
}

#[async_trait]
impl SyntaxContext for ExpressionSyntaxContext {
    type Body = Expression;
    type Statement = StatementSyntax;

    const PREFERS_LISTS: bool = true;

    fn new(ast_builder: AstBuilder) -> Self {
        ExpressionSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
    }

    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }

    fn block_scope(&self, scope: ScopeId) -> ScopeId {
        self.ast_builder.child_scope(scope)
    }

    async fn build_block(
        self,
        span: parse::SpanList,
        statements: impl Iterator<
                Item = Result<
                    <<Self::Statement as Syntax>::Context as SyntaxContext>::Body,
                    SyntaxError,
                >,
            > + Send,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        Ok(BlockExpression {
            span,
            statements: statements.collect(),
            scope,
        }
        .into())
    }

    async fn build_terminal(
        self,
        expr: parse::Expr,
        scope: ScopeId,
    ) -> Result<Self::Body, SyntaxError> {
        match expr.try_into_list_exprs() {
            Ok((span, exprs)) => self.expand_list(span, exprs.collect(), scope).await,
            Err(expr) => match expr.kind {
                parse::ExprKind::Name(name, name_scope) => Ok(NameExpression {
                    span: expr.span,
                    name,
                    scope: name_scope.unwrap_or(scope),
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
                parse::ExprKind::Block(_) => {
                    self.ast_builder
                        .build_expr::<ExpressionSyntax>(self.clone(), expr, self.block_scope(scope))
                        .await
                }
                _ => {
                    self.ast_builder.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(expr.span, "expected expression")],
                    );

                    Err(self.ast_builder.syntax_error(expr.span))
                }
            },
        }
    }
}

impl ExpressionSyntaxContext {
    async fn expand_list(
        &self,
        list_span: SpanList,
        mut exprs: Vec<parse::Expr>,
        scope: ScopeId,
    ) -> Result<Expression, SyntaxError> {
        match exprs.len() {
            0 => Ok(UnitExpression { span: list_span }.into()),
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name, name_scope) = expr.kind {
                    if let Some(syntax) = self.ast_builder.try_get_syntax(
                        name,
                        expr.span,
                        name_scope.unwrap_or(scope),
                    ) {
                        match syntax.operator_precedence {
                            Some(_) => {
                                self.ast_builder.compiler.add_error(
                                    "expected values on both sides of operator",
                                    vec![Note::primary(
                                        expr.span,
                                        "try providing values on either side of this",
                                    )],
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
                    let mut exprs = exprs.into_iter();
                    let first = exprs.next().unwrap();

                    if let parse::ExprKind::Name(name, name_scope) = first.kind {
                        if let Some(syntax) = self.ast_builder.try_get_syntax(
                            name,
                            first.span,
                            name_scope.unwrap_or(scope),
                        ) {
                            return self
                                .expand_syntax(
                                    first.span,
                                    syntax,
                                    std::iter::once(first).chain(exprs).collect(),
                                    scope,
                                )
                                .await;
                        }
                    }

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
                                    self.ast_builder.compiler.add_error(
                                        "operator ambiguity",
                                        vec![
                                            Note::primary(
                                                exprs[index].span,
                                                "only one of this operator may be provided at a time",
                                            ),
                                            Note::secondary(
                                                exprs[max_index].span,
                                                "first use of this operator",
                                            ),
                                        ],
                                    );

                                    return Err(self.ast_builder.syntax_error(list_span));
                                }
                            },
                        }
                    }

                    let rhs = exprs.split_off(max_index + 1);
                    let mut lhs = exprs;
                    let operator = lhs.pop().unwrap();

                    if rhs.is_empty() {
                        self.ast_builder.compiler.add_error(
                            "expected values on right side of operator",
                            vec![Note::primary(
                                max_expr.span,
                                "try providing a value to the right of this",
                            )],
                        );

                        Err(self.ast_builder.syntax_error(list_span))
                    } else if lhs.is_empty() {
                        self.ast_builder.compiler.add_error(
                            "expected values on left side of operator",
                            vec![Note::primary(
                                max_expr.span,
                                "try providing a value to the left of this",
                            )],
                        );

                        Err(self.ast_builder.syntax_error(list_span))
                    } else {
                        let lhs_span =
                            SpanList::join(lhs.first().unwrap().span, lhs.last().unwrap().span);

                        let lhs = parse::Expr {
                            span: lhs_span,
                            kind: parse::ExprKind::List(vec![lhs.into()]),
                        };

                        let rhs_span =
                            SpanList::join(rhs.first().unwrap().span, rhs.last().unwrap().span);

                        let rhs = parse::Expr {
                            span: rhs_span,
                            kind: parse::ExprKind::List(vec![rhs.into()]),
                        };

                        self.expand_syntax(list_span, max_syntax, vec![lhs, operator, rhs], scope)
                            .await
                    }
                }
            }
        }
    }

    fn operators_in_list<'a>(
        &'a self,
        exprs: impl IntoIterator<Item = (usize, &'a parse::Expr)>,
        scope: ScopeId,
    ) -> Vec<(
        usize,
        parse::Expr,
        SyntaxAssignmentValue,
        OperatorPrecedenceStatementAttributeKind,
    )> {
        exprs
            .into_iter()
            .filter_map(move |(index, expr)| {
                if let parse::ExprKind::Name(name, name_scope) = expr.kind {
                    if let Some(syntax) = self.ast_builder.try_get_syntax(
                        name,
                        expr.span,
                        name_scope.unwrap_or(scope),
                    ) {
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
        span: SpanList,
        syntax: SyntaxAssignmentValue,
        exprs: Vec<parse::Expr>,
        scope: ScopeId,
    ) -> Result<Expression, SyntaxError> {
        let SyntaxBody::Block(body) = syntax.body?;

        for rule in body.rules.into_iter().flatten() {
            let SyntaxRule::Function(rule) = rule;

            let (pattern, body) = match (rule.pattern, rule.body) {
                (Ok(pattern), Ok(body)) => (pattern, *body),
                _ => continue,
            };

            let vars = match SyntaxPattern::r#match(&self.ast_builder, pattern, &exprs) {
                Some(vars) => vars,
                None => continue,
            };

            let body = SyntaxPattern::expand(&self.ast_builder, body, &vars, span)?;

            return self
                .ast_builder
                .build_expr::<ExpressionSyntax>(self.clone(), body, scope)
                .await;
        }

        self.ast_builder.compiler.add_error(
            "syntax error",
            vec![
                Note::primary(span, "this does not match any syntax rules"),
                Note::secondary(syntax.syntax_span, "syntax defined here"),
            ],
        );

        Err(self.ast_builder.syntax_error(span))
    }
}
