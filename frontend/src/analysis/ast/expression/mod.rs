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
        syntax::{FileBodySyntaxContext, Syntax, SyntaxContext, SyntaxError},
        AstBuilder, OperatorAssociativity, OperatorPrecedenceStatementAttributeKind, Statement,
        StatementAttributes, StatementSyntax, SyntaxAssignmentValue,
    },
    diagnostics::Note,
    helpers::{InternedString, Shared},
    parse::{self, Span},
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
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NameExpression {
    pub span: Span,
    pub name: InternedString,
    pub scope: ScopeId,
}

#[derive(Debug, Clone)]
pub struct TextExpression {
    pub span: Span,
    pub text: InternedString,
}

#[derive(Debug, Clone)]
pub struct NumberExpression {
    pub span: Span,
    pub number: InternedString,
}

#[derive(Debug, Clone)]
pub struct CallExpression {
    pub span: Span,
    pub function: Result<Box<Expression>, SyntaxError>,
    pub inputs: Vec<Result<Expression, SyntaxError>>,
}

#[derive(Debug, Clone)]
pub struct BlockExpression {
    pub span: Span,
    pub statements: Vec<Result<Statement, SyntaxError>>,
    pub scope: ScopeId,
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

    fn new(ast_builder: AstBuilder) -> Self {
        ExpressionSyntaxContext {
            ast_builder,
            statement_attributes: None,
        }
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
    ) -> Result<Self::Body, SyntaxError> {
        let scope = self.ast_builder.child_scope(scope);

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
        // let syntax_pattern = match expr.try_into_list_exprs() {
        //     Ok((span, exprs)) => {
        //         let exprs = stream::iter(exprs)
        //             .then(|expr| {
        //                 self.ast_builder
        //                     .build_expr::<ExpressionSyntax>(self.clone(), expr, scope)
        //             })
        //             .collect::<Vec<_>>()
        //             .await;

        //         ListSyntaxPattern { span, exprs }.into()
        //     }
        //     Err(expr) => match expr.kind {
        //         parse::ExprKind::Name(name) => NameExpression {
        //             span: expr.span,
        //             name,
        //             scope,
        //         }
        //         .into(),
        //         parse::ExprKind::Text(text) => TextExpression {
        //             span: expr.span,
        //             text,
        //         }
        //         .into(),
        //         parse::ExprKind::Number(number) => NumberExpression {
        //             span: expr.span,
        //             number,
        //         }
        //         .into(),
        //         _ => {
        //             self.ast_builder.compiler.add_error(
        //                 "syntax error",
        //                 vec![Note::primary(expr.span, "expected expression")],
        //             );

        //             return Err(self.ast_builder.syntax_error(expr.span));
        //         }
        //     },
        // };

        // Ok(expr)
        todo!()
    }
}

impl FileBodySyntaxContext for ExpressionSyntaxContext {
    fn with_statement_attributes(mut self, attributes: Shared<StatementAttributes>) -> Self {
        self.statement_attributes = Some(attributes);
        self
    }
}

impl ExpressionSyntaxContext {
    async fn expand_list(
        &self,
        list_span: Span,
        mut exprs: Vec<parse::Expr>,
        scope: ScopeId,
    ) -> Result<Expression, SyntaxError> {
        match exprs.len() {
            0 => Ok(UnitExpression { span: list_span }.into()),
            1 => {
                let expr = exprs.pop().unwrap();

                if let parse::ExprKind::Name(name, name_scope) = expr.kind {
                    if let Some((syntax_span, syntax)) = self
                        .ast_builder
                        .try_get_syntax(name, name_scope.unwrap_or(scope))
                    {
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
                                return self.expand_syntax(
                                    name,
                                    expr.span,
                                    (syntax_span, syntax),
                                    Vec::new(),
                                    scope,
                                );
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
                        if let Some((syntax_span, syntax)) = self
                            .ast_builder
                            .try_get_syntax(name, name_scope.unwrap_or(scope))
                        {
                            return self.expand_syntax(
                                name,
                                first.span,
                                (syntax_span, syntax),
                                Vec::new(),
                                scope,
                            );
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

                    let (
                        mut max_index,
                        mut max_expr,
                        mut max_name,
                        mut max_syntax,
                        mut max_precedence,
                    ) = operators.front().cloned().unwrap();

                    for (index, expr, name, syntax, precedence) in operators.iter().skip(1).cloned()
                    {
                        macro_rules! replace {
                            () => {{
                                max_index = index;
                                max_expr = expr;
                                max_name = name;
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
                    lhs.pop().unwrap();

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
                            Span::join(lhs.first().unwrap().span, lhs.last().unwrap().span);
                        let lhs = parse::Expr::list_or_expr(lhs_span, lhs);

                        let rhs_span =
                            Span::join(rhs.first().unwrap().span, rhs.last().unwrap().span);
                        let rhs = parse::Expr::list_or_expr(rhs_span, rhs);

                        let span = Span::join(lhs_span, rhs_span);

                        self.expand_syntax(max_name, span, max_syntax, vec![lhs, rhs], scope)
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
        InternedString,
        (Span, SyntaxAssignmentValue),
        OperatorPrecedenceStatementAttributeKind,
    )> {
        exprs
            .into_iter()
            .filter_map(move |(index, expr)| {
                if let parse::ExprKind::Name(name, name_scope) = expr.kind {
                    if let Some((syntax_span, syntax)) = self
                        .ast_builder
                        .try_get_syntax(name, name_scope.unwrap_or(scope))
                    {
                        if let Some(attribute) = &syntax.operator_precedence {
                            let precedence = attribute.precedence;

                            return Some((
                                index,
                                expr.clone(),
                                name,
                                (syntax_span, syntax),
                                precedence,
                            ));
                        }
                    }
                }

                None
            })
            .collect()
    }

    fn expand_syntax(
        &self,
        name: InternedString,
        span: Span,
        (syntax_span, syntax): (Span, SyntaxAssignmentValue),
        exprs: Vec<parse::Expr>,
        scope: ScopeId,
    ) -> Result<Expression, SyntaxError> {
        // let vars = syntax_pattern.match_with(exprs).ok_or_else(|| self.syntax_error(..))?;

        todo!()
    }
}
