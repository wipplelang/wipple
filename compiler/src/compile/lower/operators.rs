use super::*;
use std::{cmp::Ordering, collections::VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[repr(u8)]
pub enum OperatorPrecedence {
    Addition = 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorAssociativity {
    Left,
    Right,
    None,
}

impl OperatorPrecedence {
    pub const fn associativity(&self) -> OperatorAssociativity {
        match self {
            OperatorPrecedence::Addition => OperatorAssociativity::Left,
        }
    }
}

impl<L: Loader> Compiler<L> {
    pub(super) fn lower_operators(
        &mut self,
        span: Span,
        mut list: Vec<parser::Expression>,
        scope: &Scope,
        info: &mut Info,
    ) -> Expression {
        match list.len() {
            0 => Expression {
                span,
                kind: ExpressionKind::Unit,
            },
            1 => {
                let expr = list.pop().unwrap();
                self.lower_expr(expr, scope, info)
            }
            _ => {
                let mut operators = Vec::new();

                for (index, expr) in list.iter().enumerate() {
                    if let parser::ExpressionKind::Name(name) = expr.kind {
                        if let Some(ScopeValue::Operator(id)) = scope.get(name, expr.span) {
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
                    if let (
                        parser::ExpressionKind::Name(ty_name),
                        parser::ExpressionKind::Block(statements),
                    ) = (&list[0].kind, &list[1].kind)
                    {
                        let fields = statements
                            .iter()
                            .filter_map(|s| match &s.kind {
                                parser::StatementKind::Assign(pattern, expr) => {
                                    match &pattern.kind {
                                        parser::PatternKind::Name(name) => Some((*name, expr)),
                                        _ => None,
                                    }
                                }
                                _ => None,
                            })
                            .collect::<Vec<_>>();

                        match scope.get(*ty_name, list[0].span) {
                            Some(ScopeValue::Type(ty)) => {
                                if fields.len() == statements.len() {
                                    return self.lower_instantiation(
                                        list[0].span,
                                        list[1].span,
                                        ty,
                                        fields,
                                        scope,
                                        info,
                                    );
                                }
                            }
                            Some(ScopeValue::TypeParameter(_)) => {
                                self.diagnostics.add(Diagnostic::error(
                                    "cannot instantiate type parameter",
                                    vec![Note::primary(
                                        list[0].span,
                                        "the actual type this represents is not known here",
                                    )],
                                ));

                                return Expression::error(span);
                            }
                            Some(ScopeValue::BuiltinType(_)) => {
                                self.diagnostics.add(Diagnostic::error(
                                    "cannot instantiate builtin type",
                                    vec![Note::primary(list[0].span, "try usng a literal instead")],
                                ));
                            }
                            _ => {}
                        }
                    }

                    let mut list = VecDeque::from(list);
                    let first = list.pop_front().unwrap();

                    list.into_iter()
                        .fold(self.lower_expr(first, scope, info), |result, next| {
                            let span = Span::join(result.span, next.span);

                            let member_name = match next.kind {
                                parser::ExpressionKind::Name(name) => Some(name),
                                _ => None,
                            };

                            let input = info.suppressing_name_resolution_errors(|info| {
                                self.lower_expr(next, scope, info)
                            });

                            Expression {
                                span,
                                kind: ExpressionKind::CallOrMember(
                                    Box::new(result),
                                    Box::new(input),
                                    member_name,
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
                            kind: ExpressionKind::CallOrMember(
                                Box::new(Expression {
                                    span: Span::join(lhs.first().unwrap().span, max_span),
                                    kind: ExpressionKind::CallOrMember(
                                        Box::new(Expression {
                                            span: max_span,
                                            kind: ExpressionKind::Constant(max_operator.body),
                                        }),
                                        Box::new(self.lower_operators(
                                            Span::join(lhs.first().unwrap().span, max_span),
                                            lhs,
                                            scope,
                                            info,
                                        )),
                                        None,
                                    ),
                                }),
                                Box::new(self.lower_operators(
                                    Span::join(max_span, rhs.last().unwrap().span),
                                    rhs,
                                    scope,
                                    info,
                                )),
                                None,
                            ),
                        }
                    }
                }
            }
        }
    }
}
