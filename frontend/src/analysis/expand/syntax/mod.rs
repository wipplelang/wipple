#![allow(clippy::module_inception)]

mod allow_overlapping_instances;
mod annotate;
mod assign;
mod comma;
mod end;
mod external;
mod format;
mod function;
mod help;
mod instance;
mod keyword;
mod language;
mod no_std;
mod on_mismatch;
mod on_unimplemented;
mod operator;
mod or;
mod recursion_limit;
mod specialize;
mod syntax;
mod r#trait;
mod r#type;
mod type_function;
mod r#use;
mod when;
mod r#where;

use crate::{
    analysis::expand::{
        Bound, Context, Expander, Pattern, PatternKind, Scope, ScopeValueKind, StatementAttributes,
        Syntax, TypeParameter,
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
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SyntaxDefinition {
    pub rules: Vec<SyntaxRule>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SyntaxRule {
    pub span: Span,
    pub pattern: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum ExpressionKind {
    Error(Backtrace),
    EmptySideEffect,
    Variable(InternedString),
    RepeatedVariable(InternedString),
    Underscore,
    Name(Option<ScopeId>, InternedString),
    Text(InternedString),
    Number(InternedString),
    List(Vec<Expression>),
    Block(Option<ScopeId>, Vec<Statement>),
    AssignToPattern(Pattern, Box<Expression>),
    Assign(Box<Expression>, Box<Expression>),
    Function(
        // HACK: Function syntax can be used in both value and type position,
        // and in type position we need to treat both sides as expressions
        // without introducing any variables. So we just store both
        // possibilities.
        Option<(Option<ScopeId>, Pattern, Box<Expression>)>,
        (Box<Expression>, Box<Expression>),
    ),
    Tuple(Vec<Expression>),
    External(Box<Expression>, Box<Expression>, Vec<Expression>),
    Annotate(Box<Expression>, Box<Expression>),
    Type(Option<Box<Expression>>),
    Trait(Option<Box<Expression>>),
    TypeFunction(
        Option<ScopeId>,
        (Vec<TypeParameter>, Vec<Bound>),
        Box<Expression>,
    ),
    Where(Box<Expression>, Box<Expression>),
    Instance(Box<Expression>),
    Use(Box<Expression>),
    When(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
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
                parse::ExprKind::Block(statements) => ExpressionKind::Block(
                    None,
                    statements
                        .into_iter()
                        .flat_map(|statement| statement.try_into().ok())
                        .collect(),
                ),
            },
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Statement {
    pub span: Span,
    pub unexpanded_attributes: Vec<Attribute>,
    pub attributes: StatementAttributes,
    pub expr: Expression,
}

impl TryFrom<parse::Statement> for Statement {
    type Error = ();

    fn try_from(mut statement: parse::Statement) -> Result<Self, Self::Error> {
        let unexpanded_attributes = mem::take(&mut statement.lines.first_mut().unwrap().attributes)
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
            .ok_or(())?
            .span
            .with_end(exprs.last().unwrap().span.end);

        Ok(Statement {
            span,
            unexpanded_attributes,
            attributes: Default::default(),
            expr: Expression {
                span,
                kind: ExpressionKind::List(exprs),
            },
        })
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Attribute {
    pub span: Span,
    pub exprs: Vec<Expression>,
}

impl From<parse::Attribute> for Attribute {
    fn from(attribute: parse::Attribute) -> Self {
        Attribute {
            span: attribute.span,
            exprs: attribute.exprs.into_iter().map(From::from).collect(),
        }
    }
}

impl Expression {
    pub(crate) fn unify(&self, other: &Expression) -> Option<HashMap<InternedString, Expression>> {
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
                        statement.unexpanded_attributes.len() == other.unexpanded_attributes.len()
                            && statement
                                .unexpanded_attributes
                                .iter()
                                .zip(&other.unexpanded_attributes)
                                .all(|(attribute, other)| {
                                    attribute.exprs.len() == other.exprs.len()
                                        && attribute
                                            .exprs
                                            .iter()
                                            .zip(&other.exprs)
                                            .all(|(expr, other)| expr.unify_internal(other, vars))
                                })
                            && statement.expr.unify_internal(&other.expr, vars)
                    })
            }
            _ => false, // The complex expressions (assignment, functions, etc.)
                        // will not be reached because they won't have been
                        // expanded yet
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
                            vec![Note::primary(
                                expr.span,
                                "this syntax variable is not defined",
                            )],
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
            | ExpressionKind::EmptySideEffect
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
                    for attribute in &mut statement.unexpanded_attributes {
                        for expr in &mut attribute.exprs {
                            expr.traverse_mut_with_inner(context.clone(), f);
                        }
                    }

                    statement.expr.traverse_mut_with_inner(context.clone(), f);
                }
            }
            ExpressionKind::Assign(lhs, rhs)
            | ExpressionKind::Annotate(lhs, rhs)
            | ExpressionKind::Where(lhs, rhs)
            | ExpressionKind::Or(lhs, rhs) => {
                lhs.traverse_mut_with_inner(context.clone(), f);
                rhs.traverse_mut_with_inner(context, f);
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
                    expr.traverse_mut_with_inner(context, f);
                }
            }
            ExpressionKind::When(input, arms) => {
                input.traverse_mut_with_inner(context.clone(), f);
                arms.traverse_mut_with_inner(context, f);
            }
            ExpressionKind::Instance(expr)
            | ExpressionKind::TypeFunction(_, _, expr)
            | ExpressionKind::Use(expr)
            | ExpressionKind::End(expr) => {
                expr.traverse_mut_with_inner(context, f);
            }
            ExpressionKind::AssignToPattern(pattern, expr) => {
                if let PatternKind::Annotate(_, expr) | PatternKind::Where(_, expr) =
                    &mut pattern.kind
                {
                    expr.traverse_mut_with_inner(context.clone(), f);
                }

                expr.traverse_mut_with_inner(context, f);
            }
            ExpressionKind::Function(pattern, (lhs, rhs)) => {
                if let Some((_, pattern, expr)) = pattern {
                    if let PatternKind::Annotate(_, expr) | PatternKind::Where(_, expr) =
                        &mut pattern.kind
                    {
                        expr.traverse_mut_with_inner(context.clone(), f);
                    }

                    expr.traverse_mut_with_inner(context.clone(), f);
                }

                lhs.traverse_mut_with_inner(context.clone(), f);
                rhs.traverse_mut_with_inner(context, f);
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
    fn name(self) -> &'static str;

    fn kind(self, syntax: Syntax) -> ScopeValueKind {
        ScopeValueKind::Syntax(syntax)
    }

    fn pattern(self) -> Vec<Expression>;

    async fn expand(
        self,
        span: Span,
        vars: HashMap<InternedString, Expression>,
        context: Option<Context<'_>>,
        scope: ScopeId,
        expander: &Expander<'_, '_>,
    ) -> Expression;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumIter)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[enum_dispatch(BuiltinSyntaxVisitor)]
pub enum BuiltinSyntax {
    AllowOverlappingInstances(allow_overlapping_instances::AllowOverlappingInstancesSyntax),
    Annotate(annotate::AnnotateSyntax),
    Assign(assign::AssignSyntax),
    Comma(comma::CommaSyntax),
    End(end::EndSyntax),
    External(external::ExternalSyntax),
    Format(format::FormatSyntax),
    Function(function::FunctionSyntax),
    Help(help::HelpSyntax),
    Instance(instance::InstanceSyntax),
    Keyword(keyword::KeywordSyntax),
    Language(language::LanguageSyntax),
    NoStd(no_std::NoStdSyntax),
    OnMismatch(on_mismatch::OnMismatchSyntax),
    OnUnimplemented(on_unimplemented::OnUnimplementedSyntax),
    Operator(operator::OperatorSyntax),
    Or(or::OrSyntax),
    RecursionLimit(recursion_limit::RecursionLimitSyntax),
    Specialize(specialize::SpecializeSyntax),
    Syntax(syntax::SyntaxSyntax),
    Trait(r#trait::TraitSyntax),
    Type(r#type::TypeSyntax),
    TypeFunction(type_function::TypeFunctionSyntax),
    Use(r#use::UseSyntax),
    When(when::WhenSyntax),
    Where(r#where::WhereSyntax),
}

impl BuiltinSyntax {
    pub(super) fn load_into(scope: &mut Scope) {
        for item in BuiltinSyntax::iter() {
            scope.values.insert(
                InternedString::new(item.name()),
                item.kind(Syntax::Builtin(item)),
            );
        }
    }
}
