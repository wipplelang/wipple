use crate::lower::*;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct ListExpr {
    pub span: Span,
    pub items: Vec<SpannedExpr>,
}

impl ListExpr {
    pub fn new(span: Span, items: Vec<SpannedExpr>) -> Self {
        ListExpr { span, items }
    }

    pub fn infer_span(items: Vec<SpannedExpr>) -> Self {
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

    fn lower_to_form(self, stack: Stack, diagnostics: &mut Diagnostics) -> SpannedForm {
        let span = self.span;

        let form = match self.parse_operators(stack, diagnostics) {
            Ok(operators) => operators,
            Err(error) => {
                diagnostics.add(error.into());

                return SpannedItem::new(span, Item::Error).into();
            }
        };

        match form {
            ParseResult::List(mut list_expr) => match list_expr.items.len() {
                0 => SpannedItem::unit(list_expr.span).into(),
                1 => list_expr.items.remove(0).lower_to_form(stack, diagnostics),
                _ => {
                    let form = list_expr.items.remove(0).lower_to_form(stack, diagnostics);

                    match form.form {
                        Form::Item(item) => {
                            let mut acc = SpannedItem::new(form.span, item);

                            while !list_expr.items.is_empty() {
                                let input =
                                    list_expr.items.remove(0).lower_to_item(stack, diagnostics);

                                acc = SpannedItem::apply(
                                    acc.span.with_end(input.span.end),
                                    acc,
                                    input,
                                )
                            }

                            acc.into()
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
                diagnostics,
            ),
            ParseResult::PartiallyApplyLeft(_, (operator_span, _))
            | ParseResult::PartiallyApplyRight((operator_span, _), _) => {
                diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Partial application of operators is currently unsupported",
                    vec![Note::primary(
                        operator_span,
                        "Try adding an expression on both sides of this",
                    )],
                ));

                SpannedItem::error(span).into()
            }
        }
    }

    fn lower_to_binding(mut self, stack: Stack, diagnostics: &mut Diagnostics) -> SpannedBinding {
        // eventually, support more complex expressions with operators
        if self.items.len() != 1 {
            diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "Assigning to complex expressions is not yet supported",
                vec![Note::primary(self.span, "Expected a name here")],
            ));

            return SpannedBinding::error(self.span);
        }

        let inner_expr = self.items.remove(0);

        inner_expr.lower_to_binding(stack, diagnostics)
    }
}

enum ParseResult {
    List(ListExpr),
    Apply(Vec<SpannedExpr>, (Span, OperatorForm), Vec<SpannedExpr>),
    PartiallyApplyLeft(Vec<SpannedExpr>, (Span, OperatorForm)),
    PartiallyApplyRight((Span, OperatorForm), Vec<SpannedExpr>),
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
    fn parse_operators(
        mut self,
        stack: Stack,
        diagnostics: &mut Diagnostics,
    ) -> Result<ParseResult, Error> {
        if self.items.len() <= 1 {
            return Ok(ParseResult::List(self));
        }

        let mut operators = Vec::new();

        for (index, expr) in self.items.iter().enumerate() {
            if let SpannedExpr::Name(name) = expr {
                if let Some(form) = name.resolve(stack, diagnostics) {
                    if let Form::Operator(operator) = form.form {
                        operators.push((index, form.span, operator));
                    }
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
