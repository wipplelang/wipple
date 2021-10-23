use crate::lower::*;
use std::cmp::Ordering;

pub struct ListExpr {
    pub span: Span,
    pub items: Vec<Expr>,
}

impl ListExpr {
    pub fn new(span: Span, items: Vec<Expr>) -> Self {
        ListExpr { span, items }
    }

    pub fn infer_span(items: Vec<Expr>) -> Self {
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

impl ExprKind for ListExpr {
    fn span(&self) -> Span {
        self.span
    }

    fn lower_to_form(self, stack: &Stack, info: &mut Info) -> Form {
        let span = self.span;

        let form = match self.parse_operators(stack, info) {
            Ok(operators) => operators,
            Err(error) => {
                info.diagnostics.add(error.into());

                return Form::Item(Item::new(span, ItemKind::Error));
            }
        };

        match form {
            ParseResult::List(mut list_expr) => match list_expr.items.len() {
                0 => Form::Item(Item::unit(list_expr.span)),
                1 => list_expr.items.remove(0).lower_to_form(stack, info),
                _ => {
                    let form = list_expr.items.remove(0).lower_to_form(stack, info);

                    match form {
                        Form::Item(item) => {
                            Form::Item(list_expr.items.into_iter().fold(item, |function, expr| {
                                let input = expr.lower_to_item(stack, info);
                                Item::apply(function.span.with_end(input.span.end), function, input)
                            }))
                        }
                        Form::Template(template) => todo!(),
                        Form::Operator(_) => unreachable!(),
                    }
                }
            },
            ParseResult::Apply(lhs, (_, operator), rhs) => (operator.apply)(
                ListExpr::infer_span(lhs),
                ListExpr::infer_span(rhs),
                stack,
                info,
            ),
            ParseResult::PartiallyApplyLeft(_, (operator_span, _))
            | ParseResult::PartiallyApplyRight((operator_span, _), _) => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Partial application of operators is currently unsupported",
                    vec![Note::primary(
                        operator_span,
                        "Try adding an expression on both sides of this",
                    )],
                ));

                Form::Item(Item::error(span))
            }
        }
    }

    fn lower_to_binding(mut self, stack: &Stack, info: &mut Info) -> Option<SpannedBinding> {
        // eventually, support more complex expressions with operators
        if self.items.len() != 1 {
            return None;
        }

        let inner_expr = self.items.remove(0);

        inner_expr.lower_to_binding(stack, info)
    }
}

enum ParseResult {
    List(ListExpr),
    Apply(Vec<Expr>, (Span, OperatorForm), Vec<Expr>),
    PartiallyApplyLeft(Vec<Expr>, (Span, OperatorForm)),
    PartiallyApplyRight((Span, OperatorForm), Vec<Expr>),
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

        Diagnostic::new(DiagnosticLevel::Error, "Operator ambiguity", notes)
    }
}

impl ListExpr {
    fn parse_operators(mut self, stack: &Stack, info: &mut Info) -> Result<ParseResult, Error> {
        if self.items.len() <= 1 {
            return Ok(ParseResult::List(self));
        }

        let mut operators = Vec::new();

        for (index, expr) in self.items.iter().enumerate() {
            if let Expr::Name(name) = expr {
                if let Some(Form::Operator(operator)) = name.resolve(stack, info) {
                    operators.push((index, operator.span, operator));
                }
            }
        }

        if operators.is_empty() {
            return Ok(ParseResult::List(self));
        }

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
            ParseResult::PartiallyApplyLeft(lhs, (max_span, max_operator))
        } else if lhs.is_empty() {
            ParseResult::PartiallyApplyRight((max_span, max_operator), rhs)
        } else {
            ParseResult::Apply(lhs, (max_span, max_operator), rhs)
        })
    }
}
