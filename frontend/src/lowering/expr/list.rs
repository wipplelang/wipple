use crate::{
    diagnostics::{Diagnostic, DiagnosticLevel, Diagnostics, Note},
    lowering::*,
    parser::Span,
};
use std::cmp::Ordering;

#[derive(Debug)]
pub struct ListExpr {
    pub span: Span,
    pub items: Vec<AnyExpr>,
}

impl ListExpr {
    pub fn new(span: Span, items: Vec<AnyExpr>) -> Self {
        ListExpr { span, items }
    }

    pub fn infer_span(items: Vec<AnyExpr>) -> Self {
        ListExpr::new(
            items
                .first()
                .unwrap()
                .span()
                .with_end(items.last().unwrap().span().end),
            items,
        )
    }
}

impl Expr for ListExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower(self, scope: &mut Scope, diagnostics: &mut Diagnostics) -> LoweredExpr {
        let span = self.span;

        let form = match self.parse_operators(scope, diagnostics) {
            Ok(operators) => operators,
            Err(error) => {
                diagnostics.add(error.into());

                return LoweredExpr::new(span, LoweredExprKind::Error(LoweredErrorExpr::new()));
            }
        };

        match form {
            Form::List(mut list_expr) => match list_expr.items.len() {
                0 => LoweredExpr::new(
                    list_expr.span,
                    LoweredExprKind::Unit(LoweredUnitExpr::new()),
                ),
                1 => list_expr.items.remove(0).lower(scope, diagnostics),
                _ => {
                    let mut acc = list_expr.items.remove(0).lower(scope, diagnostics);

                    while !list_expr.items.is_empty() {
                        acc = match acc.kind {
                            LoweredExprKind::Builtin(builtin) => {
                                builtin.apply(acc.span, &mut list_expr.items, diagnostics)
                            }
                            // eventually, templates
                            _ => {
                                let input = list_expr.items.remove(0).lower(scope, diagnostics);

                                LoweredExpr::new(
                                    acc.span.with_end(input.span.end),
                                    LoweredExprKind::Apply(LoweredApplyExpr::new(acc, input)),
                                )
                            }
                        };
                    }

                    acc
                }
            },
            Form::Apply(lhs, operator, rhs) => (operator.apply)(
                ListExpr::infer_span(lhs),
                ListExpr::infer_span(rhs),
                scope,
                diagnostics,
            ),
            Form::PartiallyApplyLeft(lhs, operator) => {
                (operator.partially_apply_left)(ListExpr::infer_span(lhs), scope, diagnostics)
            }
            Form::PartiallyApplyRight(operator, rhs) => {
                (operator.partially_apply_right)(ListExpr::infer_span(rhs), scope, diagnostics)
            }
        }
    }

    fn binding(mut self) -> Option<Binding> {
        // eventually, support more complex expressions with operators
        if self.items.len() != 1 {
            return None;
        }

        let inner_expr = self.items.remove(0);

        inner_expr.binding()
    }
}

enum Form {
    List(ListExpr),
    Apply(Vec<AnyExpr>, Operator, Vec<AnyExpr>),
    PartiallyApplyLeft(Vec<AnyExpr>, Operator),
    PartiallyApplyRight(Operator, Vec<AnyExpr>),
    // TODO: partial application
}

enum Error {
    AmbiguousAssociativity { first: Span, second: Span },
    MultipleNonAssociativeOperators { first: Span, second: Span },
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        let notes = match error {
            Error::AmbiguousAssociativity { first, second } => vec![
                Note::primary(first, "Ambiguous whether to parse this operator first..."),
                Note::primary(second, "...or this one"),
            ],
            Error::MultipleNonAssociativeOperators { first, second } => vec![
                Note::primary(
                    second,
                    "Only one of this operator may be provided at a time",
                ),
                Note::secondary(first, "First use of this operator"),
            ],
        };

        Diagnostic::new(DiagnosticLevel::Error, "Syntax error", notes)
    }
}

impl ListExpr {
    fn parse_operators(
        mut self,
        scope: &Scope,
        diagnostics: &mut Diagnostics,
    ) -> Result<Form, Error> {
        let mut operators = Vec::new();

        for (index, expr) in self.items.iter().enumerate() {
            if let Some(operator) = expr.operator(scope, diagnostics) {
                operators.push((index, operator));
            }
        }

        if operators.is_empty() {
            return Ok(Form::List(self));
        }

        let (mut max_index, mut max_operator) = operators.remove(0);

        for (index, operator) in operators {
            macro_rules! replace {
                () => {{
                    max_index = index;
                    max_operator = operator;
                }};
            }

            match operator.precedence.cmp(&max_operator.precedence) {
                Ordering::Greater => replace!(),
                Ordering::Equal => {
                    if operator.associativity != max_operator.associativity {
                        return Err(Error::AmbiguousAssociativity {
                            first: self.items[index].span(),
                            second: self.items[max_index].span(),
                        });
                    }

                    match operator.associativity {
                        OperatorAssociativity::Left => {
                            if index < max_index {
                                replace!();
                            }
                        }
                        OperatorAssociativity::Right => {
                            if index > max_index {
                                replace!();
                            }
                        }
                        OperatorAssociativity::None => {
                            return Err(Error::MultipleNonAssociativeOperators {
                                first: self.items[index].span(),
                                second: self.items[max_index].span(),
                            })
                        }
                    }
                }
                Ordering::Less => continue,
            }
        }

        let rhs = self.items.split_off(max_index + 1);

        let mut lhs = self.items;
        lhs.pop().unwrap();

        Ok(if rhs.is_empty() {
            Form::PartiallyApplyLeft(lhs, max_operator.operator)
        } else if lhs.is_empty() {
            Form::PartiallyApplyRight(max_operator.operator, rhs)
        } else {
            Form::Apply(lhs, max_operator.operator, rhs)
        })
    }
}
