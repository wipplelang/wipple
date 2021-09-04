//! Resolve a parsed expression into a format suitable for the typechecker.

use crate::{diagnostics::Diagnostics, parser};
use codemap_diagnostic::Level;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

id! {
    /// A unique identifier for a variable.
    pub VariableId;
}

id! {
    /// A unique identifier for an expression.
    pub ExpressionId;
}

macro_rules! expression_type {
    ($(#[$meta:meta])* $vis:vis $t:ident of $kind:ty $(;)?) => {
        $(#[$meta])*
        #[derive(Debug, Clone)]
        $vis struct $t {
            /// The unique identifier of the expression.
            pub id: ExpressionId,

            /// The expression's location in the source code.
            pub location: parser::Location,

            /// The kind of expression.
            pub kind: $kind,
        }
    };
}

expression_type! {
    /// A resolved expression.
    pub Expression of ExpressionKind;
}

macro_rules! expression_kind {
    ($(#[$meta:meta])* $vis:vis $t:ident of $e:ident $({ .., $($v:tt)* })?) => {
        $(#[$meta])*
        #[derive(Debug, Clone)]
        $vis enum $t {
            /// A function call, eg. `f x`.
            Call(Box<$e>, Box<$e>),

            /// A block, eg. `{ a b c }`.
            Block(Vec<$e>),

            $($($v)*)?
        }
    };
}

expression_kind! {
    /// The kind of a resolved expression.
    pub ExpressionKind of Expression {
        ..,

        /// A constant value, eg. `42`.
        Constant(parser::ExpressionKind),

        /// A variable, eg. `x`.
        Variable(VariableId),
    }
}

expression_kind!(InnerPartialExpressionKind of PartialExpression);

expression_type! {
    ///
    pub UnresolvedExpression of UnresolvedExpressionKind;
}

parser_expression_kind! {
    /// The kind of an unresolved expression.
    pub UnresolvedExpressionKind of UnresolvedExpression;
}

impl From<parser::Expression> for UnresolvedExpression {
    fn from(expr: parser::Expression) -> Self {
        let id = ExpressionId::new();

        match expr.kind {
            parser::ExpressionKind::Name(name) => UnresolvedExpression {
                id,
                location: expr.location,
                kind: UnresolvedExpressionKind::Name(name),
            },
            parser::ExpressionKind::Number(_) => todo!(),
            parser::ExpressionKind::Text(_) => todo!(),
            parser::ExpressionKind::List(_) => todo!(),
            parser::ExpressionKind::Block(_) => todo!(),
        }
    }
}

expression_type!(PartialExpression of PartialExpressionKind);

#[derive(Debug, Clone)]
enum PartialExpressionKind {
    Unresolved(UnresolvedExpressionKind),
    PartiallyResolved(InnerPartialExpressionKind),
    FullyResolved(ExpressionKind),
}

impl PartialExpression {
    fn try_into_fully_resolved(self) -> Result<Expression, Self> {
        match self.kind {
            PartialExpressionKind::FullyResolved(kind) => Ok(Expression {
                id: self.id,
                location: self.location,
                kind,
            }),
            _ => Err(self),
        }
    }
}

impl From<UnresolvedExpression> for PartialExpression {
    fn from(expr: UnresolvedExpression) -> Self {
        PartialExpression {
            id: expr.id,
            location: expr.location,
            kind: PartialExpressionKind::Unresolved(expr.kind),
        }
    }
}

impl From<Expression> for PartialExpression {
    fn from(expr: Expression) -> Self {
        PartialExpression {
            id: expr.id,
            location: expr.location,
            kind: PartialExpressionKind::FullyResolved(expr.kind),
        }
    }
}

/// Container for variables and their definitions.
#[derive(Debug, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Self>,
    variables: RefCell<HashMap<&'static str, Rc<RefCell<Variable>>>>,
}

#[derive(Debug, Clone)]
enum Variable {
    Value(PartialExpression),
    Resolving,
    Unresolvable,
}

impl<'a> Scope<'a> {
    /// Create a new scope with `self` as its parent.
    pub fn child(&'a self) -> Self {
        Scope {
            parent: Some(self),
            ..Default::default()
        }
    }
}

impl Scope<'_> {
    fn variable(&self, name: &'static str) -> Option<Rc<RefCell<Variable>>> {
        self.variables
            .borrow_mut()
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|scope| scope.variable(name)))
    }
}

/// Resolve an expression in a scope, or `None` if the expression cannot be
/// resolved.
pub fn resolve<'a>(
    expr: UnresolvedExpression,
    scope: &'a Scope<'a>,
    diagnostics: &mut Diagnostics,
) -> Option<Expression> {
    let mut unresolved = PartialExpression::from(expr);

    loop {
        let expr = resolve_expr(unresolved, scope, diagnostics)?;

        match expr.try_into_fully_resolved() {
            Ok(expr) => return Some(expr),
            Err(expr) => unresolved = expr,
        }
    }
}

fn resolve_expr<'a>(
    expr: PartialExpression,
    scope: &'a Scope<'a>,
    diagnostics: &mut Diagnostics,
) -> Option<PartialExpression> {
    match expr.kind {
        PartialExpressionKind::Unresolved(kind) => {
            let expr = UnresolvedExpression {
                id: expr.id,
                location: expr.location,
                kind,
            };

            match expr.kind {
                UnresolvedExpressionKind::Name(name) => {
                    let variable = match scope.variable(name) {
                        Some(variable) => variable,
                        None => {
                            diagnostics.add(
                                expr.clone().location,
                                Level::Error,
                                "Cannot find variable",
                            );

                            return None;
                        }
                    };

                    let new = match variable.replace(Variable::Resolving) {
                        Variable::Value(expr) => Variable::Value(expr),
                        Variable::Resolving => {
                            diagnostics.add(
                                expr.clone().location,
                                Level::Error,
                                "Variable refers to itself",
                            );

                            Variable::Unresolvable
                        }
                        Variable::Unresolvable => Variable::Unresolvable,
                    };

                    *variable.borrow_mut() = new.clone();

                    match new {
                        Variable::Value(expr) => Some(expr),
                        Variable::Resolving | Variable::Unresolvable => None,
                    }
                }
                UnresolvedExpressionKind::Number(n) => Some(PartialExpression {
                    id: expr.id,
                    location: expr.location,
                    kind: PartialExpressionKind::FullyResolved(ExpressionKind::Constant(
                        parser::ExpressionKind::Number(n),
                    )),
                }),
                UnresolvedExpressionKind::Text(t) => Some(PartialExpression {
                    id: expr.id,
                    location: expr.location,
                    kind: PartialExpressionKind::FullyResolved(ExpressionKind::Constant(
                        parser::ExpressionKind::Text(t),
                    )),
                }),
                UnresolvedExpressionKind::List(items) => {
                    let mut result = Vec::with_capacity(items.len());
                    for expr in items {
                        result.push(resolve_expr(expr.into(), scope, diagnostics));
                    }

                    todo!("Parse operators in list")
                }
                UnresolvedExpressionKind::Block(statements) => {
                    let statements = statements
                        .into_iter()
                        .map(|statement| {
                            let first = statement.first().unwrap();

                            PartialExpression {
                                id: first.id,
                                location: first.location,
                                kind: PartialExpressionKind::Unresolved(
                                    UnresolvedExpressionKind::List(statement),
                                ),
                            }
                        })
                        .collect();

                    Some(PartialExpression {
                        id: expr.id,
                        location: expr.location,
                        kind: PartialExpressionKind::PartiallyResolved(
                            InnerPartialExpressionKind::Block(statements),
                        ),
                    })
                }
            }
        }
        PartialExpressionKind::PartiallyResolved(kind) => match kind {
            InnerPartialExpressionKind::Call(func, input) => todo!(),
            InnerPartialExpressionKind::Block(statements) => {
                let scope = scope.child();
                let mut is_fully_resolved = true;

                let statements = statements
                    .into_iter()
                    .map(|expr| {
                        let expr = resolve_expr(expr, &scope, diagnostics)?;

                        if !matches!(expr.kind, PartialExpressionKind::FullyResolved(_)) {
                            is_fully_resolved = false;
                        }

                        Some(expr)
                    })
                    .collect::<Option<Vec<_>>>()?;

                Some(PartialExpression {
                    id: expr.id,
                    location: expr.location,
                    kind: if is_fully_resolved {
                        PartialExpressionKind::FullyResolved(ExpressionKind::Block(
                            statements
                                .into_iter()
                                .map(|statement| statement.try_into_fully_resolved().unwrap())
                                .collect(),
                        ))
                    } else {
                        PartialExpressionKind::PartiallyResolved(InnerPartialExpressionKind::Block(
                            statements,
                        ))
                    },
                })
            }
        },
        PartialExpressionKind::FullyResolved(_) => Some(expr),
    }
}
