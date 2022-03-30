use super::*;
use std::{cmp::Ordering, collections::VecDeque};

impl OperatorPrecedence {
    pub const fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Assignment => OperatorAssociativity::Right,
            OperatorPrecedence::Function => OperatorAssociativity::Right,
            OperatorPrecedence::Field => OperatorAssociativity::Left,
            OperatorPrecedence::Annotation => OperatorAssociativity::Left,
        }
    }
}

impl<L: Loader> Compiler<L> {
    pub(super) fn lower_operators(
        &mut self,
        mut list: Vec<parser::Expression>,
        scope: &Scope,
        info: &mut Info,
    ) -> Expression {
        if list.len() == 1 {
            let expr = list.pop().unwrap();
            return self.lower_expr(expr, scope, info);
        }

        let mut operators = Vec::new();

        for (index, expr) in list.iter().enumerate() {
            if let parser::ExpressionKind::Name(name) = expr.kind {
                if let Some(ScopeValue::Operator(id)) = scope.get(name) {
                    let operator = match info.declarations.operators.get(&id).unwrap() {
                        Declaration::Local(decl) => &decl.value,
                        Declaration::Dependency(_) => {
                            unreachable!("operator declarations are copied")
                        }
                        Declaration::Builtin(decl) => &decl.value,
                    };

                    operators.push((index, expr.span, operator));
                }
            }
        }

        if operators.is_empty() {
            let mut list = VecDeque::from(list);
            let first = list.pop_front().unwrap();

            list.into_iter()
                .fold(self.lower_expr(first, scope, info), |result, next| {
                    Expression {
                        span: Span::join(result.span, next.span),
                        kind: ExpressionKind::Call(
                            Box::new(result),
                            Box::new(self.lower_expr(next, scope, info)),
                        ),
                    }
                })
        } else {
            let (mut max_index, mut max_span, mut max_operator) = operators.remove(0);

            for (index, span, operator) in operators {
                macro_rules! replace {
                    () => {{
                        max_index = index;
                        max_span = span;
                        max_operator = operator;
                    }};
                }

                match operator.precedence.cmp(&max_operator.precedence) {
                    Ordering::Greater => replace!(),
                    Ordering::Equal => {
                        if operator.precedence.associativity()
                            != max_operator.precedence.associativity()
                        {
                            self.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "operator ambiguity",
                                vec![
                                    Note::primary(
                                        list[max_index].span,
                                        "ambiguous whether to parse this operator first...",
                                    ),
                                    Note::primary(list[index].span, "...or this one"),
                                ],
                            ));

                            return Expression::error(max_span);
                        }

                        match operator.precedence.associativity() {
                            OperatorAssociativity::Left => {
                                if index > max_index {
                                    replace!();
                                }
                            }
                            OperatorAssociativity::Right => {
                                if index < max_index {
                                    replace!();
                                }
                            }
                            OperatorAssociativity::None => {
                                self.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "operator ambiguity",
                                    vec![
                                        Note::primary(
                                            list[index].span,
                                            "only one of this operator may be provided at a time",
                                        ),
                                        Note::secondary(
                                            list[max_index].span,
                                            "first use of this operator",
                                        ),
                                    ],
                                ));

                                return Expression::error(max_span);
                            }
                        }
                    }
                    Ordering::Less => continue,
                }
            }

            let rhs = list.split_off(max_index + 1);
            let mut lhs = list;
            lhs.pop().unwrap();

            if rhs.is_empty() {
                self.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "expected values on right side of operator",
                    vec![Note::primary(
                        max_span,
                        "try providing a value to the right of this",
                    )],
                ));

                Expression::error(max_span)
            } else if lhs.is_empty() {
                self.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "expected values on left side of operator",
                    vec![Note::primary(
                        max_span,
                        "try providing a value to the left of this",
                    )],
                ));

                Expression::error(max_span)
            } else {
                Expression {
                    span: Span::join(lhs.first().unwrap().span, rhs.last().unwrap().span),
                    kind: ExpressionKind::Call(
                        Box::new(Expression {
                            span: Span::join(lhs.first().unwrap().span, max_span),
                            kind: ExpressionKind::Call(
                                Box::new(Expression {
                                    span: max_span,
                                    kind: ExpressionKind::Constant(max_operator.body),
                                }),
                                Box::new(self.lower_operators(lhs, scope, info)),
                            ),
                        }),
                        Box::new(self.lower_operators(rhs, scope, info)),
                    ),
                }
            }
        }
    }
}
