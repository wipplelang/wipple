mod assign;
mod comma;
mod function;

use crate::{
    analysis::expand_v2::{
        operators::OperatorPrecedence, Expander, Operator, Scope, ScopeValueKind, Syntax,
    },
    diagnostics::Note,
    helpers::{Backtrace, InternedString},
    parse::{self, Span},
    Compiler, ScopeId,
};
use async_trait::async_trait;
use enum_dispatch::enum_dispatch;
use std::{collections::HashMap, mem};
use strum::IntoEnumIterator;

#[derive(Debug, Clone)]
pub struct SyntaxDefinition {
    pub rules: Vec<SyntaxRule>,
}

#[derive(Debug, Clone)]
pub struct SyntaxRule {
    pub span: Span,
    pub pattern: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Expression {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionKind {
    Error(Backtrace),
    Empty,
    Variable(InternedString),
    RepeatedVariable(InternedString),
    Underscore,
    Name(Option<ScopeId>, InternedString),
    Text(InternedString),
    Number(InternedString),
    List(Vec<Expression>),
    Block(Option<ScopeId>, Vec<Statement>),
    Assign(Box<Expression>, Box<Expression>),
    Function(Box<Expression>, Box<Expression>),
    Tuple(Vec<Expression>),
    External(Box<Expression>, Box<Expression>, Vec<Expression>),
    Annotate(Box<Expression>, Box<Expression>),
    Type(Option<Box<Expression>>),
    Trait(Option<Box<Expression>>),
    TypeFunction(Option<ScopeId>, Box<Expression>, Box<Expression>),
    Where(Box<Expression>, Box<Expression>),
    Instance(Box<Expression>),
    Use(Box<Expression>),
    When(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    // no Syntax, Operator, or UseFile -- these will be parsed directly by the `:` operator
    End(Box<Expression>),
}

impl ExpressionKind {
    pub(crate) fn error(compiler: &Compiler) -> Self {
        ExpressionKind::Error(compiler.backtrace())
    }
}

impl From<parse::Expr> for Expression {
    fn from(expr: parse::Expr) -> Self {
        Expression {
            span: expr.span,
            kind: match expr.kind {
                parse::ExprKind::Underscore => ExpressionKind::Underscore,
                parse::ExprKind::Name(name) => ExpressionKind::Name(None, name),
                parse::ExprKind::Text(text) => ExpressionKind::Text(text),
                parse::ExprKind::Number(number) => ExpressionKind::Number(number),
                parse::ExprKind::List(lines) => ExpressionKind::List(
                    lines
                        .into_iter()
                        .flat_map(|line| line.exprs)
                        .map(From::from)
                        .collect(),
                ),
                parse::ExprKind::Block(statements) => {
                    ExpressionKind::Block(None, statements.into_iter().map(From::from).collect())
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub attributes: Vec<Expression>,
    pub expr: Expression,
}

impl PartialEq for Statement {
    fn eq(&self, other: &Self) -> bool {
        self.attributes == other.attributes && self.expr == other.expr
    }
}

impl Eq for Statement {}

impl From<parse::Statement> for Statement {
    fn from(mut statement: parse::Statement) -> Self {
        let attributes = mem::take(&mut statement.lines.first_mut().unwrap().attributes)
            .into_iter()
            .map(From::from)
            .collect();

        let exprs = statement
            .lines
            .into_iter()
            .flat_map(|line| line.exprs)
            .map(Expression::from)
            .collect::<Vec<_>>();

        let span = exprs
            .first()
            .unwrap()
            .span
            .with_end(exprs.last().unwrap().span.end);

        Statement {
            span,
            attributes,
            expr: Expression {
                span,
                kind: ExpressionKind::List(exprs),
            },
        }
    }
}

impl Expression {
    pub(crate) fn unify<'a>(
        &self,
        other: &Expression,
    ) -> Option<HashMap<InternedString, Expression>> {
        let mut vars = HashMap::new();
        let matched = self.unify_internal(other, &mut vars);
        matched.then_some(vars)
    }

    fn unify_internal(
        &self,
        other: &Expression,
        vars: &mut HashMap<InternedString, Expression>,
    ) -> bool {
        match (&self.kind, &other.kind) {
            (ExpressionKind::Error(_), _) | (_, ExpressionKind::Error(_)) => true,
            (_, ExpressionKind::Variable(var)) => {
                vars.insert(*var, self.clone());
                true
            }
            (ExpressionKind::Variable(var), _) => {
                vars.insert(*var, other.clone());
                true
            }
            (_, ExpressionKind::RepeatedVariable(var)) => {
                vars.insert(
                    *var,
                    Expression {
                        span: self.span,
                        kind: ExpressionKind::List(vec![self.clone()]),
                    },
                );

                true
            }
            (ExpressionKind::RepeatedVariable(var), _) => {
                vars.insert(
                    *var,
                    Expression {
                        span: other.span,
                        kind: ExpressionKind::List(vec![other.clone()]),
                    },
                );

                true
            }
            (ExpressionKind::Underscore, ExpressionKind::Underscore) => true,
            (ExpressionKind::Name(_, name), ExpressionKind::Name(_, other)) => name == other,
            (ExpressionKind::Number(number), ExpressionKind::Number(other)) => number == other,
            (ExpressionKind::Text(text), ExpressionKind::Text(other)) => text == other,
            (ExpressionKind::List(exprs), ExpressionKind::List(other)) => {
                self.unify_lists(exprs, other, vars)
            }
            (ExpressionKind::Block(_, statements), ExpressionKind::Block(_, other)) => {
                statements.len() == other.len()
                    && statements.iter().zip(other).all(|(statement, other)| {
                        statement.attributes.len() == other.attributes.len()
                            && statement
                                .attributes
                                .iter()
                                .zip(&other.attributes)
                                .all(|(attribute, other)| attribute.unify_internal(other, vars))
                            && statement.expr.unify_internal(&other.expr, vars)
                    })
            }
            (ExpressionKind::Assign(lhs, rhs), ExpressionKind::Assign(other_lhs, other_rhs))
            | (
                ExpressionKind::Function(lhs, rhs),
                ExpressionKind::Function(other_lhs, other_rhs),
            ) => lhs.unify_internal(other_lhs, vars) && rhs.unify_internal(other_rhs, vars),
            (ExpressionKind::Tuple(exprs), ExpressionKind::Tuple(other)) => {
                exprs.len() == other.len()
                    && exprs
                        .iter()
                        .zip(other)
                        .all(|(expr, other)| expr.unify_internal(other, vars))
            }
            _ => false,
        }
    }

    fn unify_lists(
        &self,
        exprs: &[Expression],
        other: &[Expression],
        vars: &mut HashMap<InternedString, Expression>,
    ) -> bool {
        let mut last_span = exprs.last().map_or(self.span, |expr| expr.span);

        let mut exprs = exprs.iter().cloned();

        for other in other {
            // TODO: Complex repeated patterns (make `exprs` peekable)
            if let ExpressionKind::RepeatedVariable(var) = &other.kind {
                let exprs = exprs.collect::<Vec<_>>();

                let span = exprs
                    .first()
                    .and_then(|first| Some((first, exprs.last()?)))
                    .map(|(first, last)| {
                        last_span = last.span;
                        first.span.with_end(last.span.end)
                    })
                    .unwrap_or(last_span);

                vars.insert(
                    *var,
                    Expression {
                        span,
                        kind: ExpressionKind::List(exprs),
                    },
                );

                return true;
            }

            let expr = match exprs.next() {
                Some(expr) => expr,
                None => return false,
            };

            if !expr.unify_internal(other, vars) {
                return false;
            }
        }

        true
    }
}

impl Expression {
    pub(crate) fn expand(
        &mut self,
        vars: &HashMap<InternedString, Expression>,
        expander: &Expander,
    ) {
        self.traverse_mut(|expr| match &mut expr.kind {
            ExpressionKind::Variable(var) => {
                *expr = match vars.get(var) {
                    Some(expr) => expr.clone(),
                    None => {
                        expander.compiler.add_error(
                            format!("cannot find `{}`", var),
                            vec![Note::primary(expr.span, "this name is not defined")],
                        );

                        expr.kind = ExpressionKind::error(expander.compiler);

                        return;
                    }
                };
            }
            ExpressionKind::RepeatedVariable(_) => {
                expander.compiler.add_error(
                    "repeated syntax variables are not allowed here",
                    vec![Note::primary(expr.span, "try removing this")],
                );

                expr.kind = ExpressionKind::error(expander.compiler);
            }
            _ => {}
        })
    }
}

impl Expression {
    pub(crate) fn traverse_mut(&mut self, mut f: impl FnMut(&mut Expression)) {
        self.traverse_mut_with_inner((), &mut |expr, ()| f(expr));
    }

    pub(crate) fn traverse_mut_with<T: Clone>(
        &mut self,
        context: T,
        mut f: impl FnMut(&mut Expression, T) -> T,
    ) {
        self.traverse_mut_with_inner(context, &mut f);
    }

    fn traverse_mut_with_inner<T: Clone>(
        &mut self,
        context: T,
        f: &mut impl FnMut(&mut Expression, T) -> T,
    ) {
        let context = f(self, context);

        match &mut self.kind {
            ExpressionKind::Error(_)
            | ExpressionKind::Empty
            | ExpressionKind::Variable(_)
            | ExpressionKind::RepeatedVariable(_)
            | ExpressionKind::Underscore
            | ExpressionKind::Name(_, _)
            | ExpressionKind::Text(_)
            | ExpressionKind::Number(_) => {}
            ExpressionKind::List(exprs) => {
                for expr in exprs {
                    expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::Block(_, statements) => {
                for statement in statements {
                    for attribute in &mut statement.attributes {
                        attribute.traverse_mut_with_inner(context.clone(), f);
                    }

                    statement.expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::Assign(lhs, rhs)
            | ExpressionKind::Function(lhs, rhs)
            | ExpressionKind::Annotate(lhs, rhs)
            | ExpressionKind::TypeFunction(_, lhs, rhs)
            | ExpressionKind::Where(lhs, rhs)
            | ExpressionKind::Or(lhs, rhs) => {
                lhs.traverse_mut_with_inner(context.clone(), f);
                rhs.traverse_mut_with_inner(context.clone(), f);
            }
            ExpressionKind::Tuple(exprs) => {
                for expr in exprs {
                    expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::External(namespace, identifier, inputs) => {
                namespace.traverse_mut_with_inner(context.clone(), f);
                identifier.traverse_mut_with_inner(context.clone(), f);

                for expr in inputs {
                    expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::Type(expr) | ExpressionKind::Trait(expr) => {
                if let Some(expr) = expr {
                    expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::When(input, arms) => {
                input.traverse_mut_with_inner(context.clone(), f);
                arms.traverse_mut_with_inner(context.clone(), f);
            }
            ExpressionKind::Instance(expr)
            | ExpressionKind::Use(expr)
            | ExpressionKind::End(expr) => {
                expr.traverse_mut_with_inner(context.clone(), f);
            }
        }
    }
}

#[async_trait]
#[enum_dispatch]
pub(super) trait BuiltinSyntaxVisitor
where
    Self: Copy + Send,
{
    fn kind(self, syntax: Syntax) -> ScopeValueKind;

    fn pattern(self) -> Expression;

    async fn expand(
        self,
        span: Span,
        vars: HashMap<InternedString, Expression>,
        expander: &Expander<'_, '_>,
    ) -> Expression;
}

#[async_trait]
pub(super) trait BuiltinOperatorVisitor
where
    Self: Copy + Send,
{
    const PRECEDENCE: OperatorPrecedence;
    const CONSTRUCT: BuiltinOperatorVisitorConstructor;
}

pub(super) enum BuiltinOperatorVisitorConstructor {
    Binary(fn(Span, Expression, Expression) -> Expression),
    Variadic(fn(Span, Vec<Expression>) -> Expression),
}

#[async_trait]
impl<T: BuiltinOperatorVisitor> BuiltinSyntaxVisitor for T {
    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Operator(Operator {
            precedence: Self::PRECEDENCE,
            syntax,
        })
    }

    fn pattern(self) -> Expression {
        match Self::CONSTRUCT {
            BuiltinOperatorVisitorConstructor::Binary(c) => c(
                Span::builtin(),
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Variable(InternedString::new("lhs")),
                },
                Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::Variable(InternedString::new("rhs")),
                },
            ),
            BuiltinOperatorVisitorConstructor::Variadic(c) => c(
                Span::builtin(),
                vec![Expression {
                    span: Span::builtin(),
                    kind: ExpressionKind::RepeatedVariable(InternedString::new("exprs")),
                }],
            ),
        }
    }

    async fn expand(
        self,
        span: Span,
        mut vars: HashMap<InternedString, Expression>,
        _expander: &Expander<'_, '_>,
    ) -> Expression {
        match Self::CONSTRUCT {
            BuiltinOperatorVisitorConstructor::Binary(c) => {
                let lhs = vars.remove("lhs").unwrap();
                let rhs = vars.remove("rhs").unwrap();

                c(span, lhs, rhs)
            }
            BuiltinOperatorVisitorConstructor::Variadic(c) => {
                let exprs = match vars.remove("exprs").unwrap().kind {
                    ExpressionKind::List(exprs) => exprs,
                    _ => unreachable!(),
                };

                c(span, exprs)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumIter, strum::AsRefStr)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[enum_dispatch(BuiltinSyntaxVisitor)]
pub enum BuiltinSyntax {
    Assign(assign::AssignSyntax),
    Comma(comma::CommaSyntax),
    Function(function::FunctionSyntax),
}

impl BuiltinSyntax {
    pub(super) fn load_into(scope: &mut Scope) {
        for item in BuiltinSyntax::iter() {
            scope.values.insert(
                InternedString::new(item),
                ScopeValueKind::Syntax(Syntax::Builtin(item)),
            );
        }
    }
}
