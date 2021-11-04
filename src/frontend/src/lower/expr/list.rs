use crate::{lower::*, typecheck::Ty};
use std::cmp::Ordering;

#[derive(Debug)]
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

                return Form::item(span, Item::new(span, ItemKind::Error));
            }
        };

        match form {
            ParseResult::List(mut list_expr) => match list_expr.items.len() {
                0 => Form::item(list_expr.span, Item::unit(list_expr.span)),
                1 => list_expr.items.remove(0).lower_to_form(stack, info),
                _ => {
                    let form = list_expr.items.remove(0).lower_to_form(stack, info);

                    list_expr
                        .items
                        .into_iter()
                        .fold(form, |form, expr| match form.kind {
                            FormKind::Item { item } => {
                                let input = expr.lower_to_item(stack, info);
                                let span = item.debug_info.span.with_end(input.debug_info.span.end);

                                Form::item(span, Item::apply(span, Box::new(item), Box::new(input)))
                            }
                            FormKind::Template { .. } => todo!(),
                            FormKind::Operator { .. } => {
                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Expected value, found operator",
                                    vec![Note::primary(form.span, "Expected value here")],
                                ));

                                Form::item(form.span, Item::error(form.span))
                            }
                            FormKind::Ty { .. } => todo!(), // constructors
                            FormKind::File { file } => {
                                let span = expr.span();

                                let name = match expr {
                                    Expr::Name(name) => name,
                                    _ => {
                                        info.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "Expected name",
                                            vec![Note::primary(
                                                form.span,
                                                "Expected variable name here",
                                            )],
                                        ));

                                        return Form::item(span, Item::error(span));
                                    }
                                };

                                if let Some(variable) = file.variables.get(&name.value) {
                                    if let Some(form) = &variable.form {
                                        form(span, info)
                                    } else {
                                        info.used_variables.borrow_mut().insert(variable.id);

                                        let mut item = Item::variable(span, variable.id);
                                        item.debug_info.declared_name = Some(name.value);
                                        Form::item(span, item)
                                    }
                                } else {
                                    info.diagnostics.add(Diagnostic::new(
                                        DiagnosticLevel::Error,
                                        format!("'{}' is not defined inside file", name.value),
                                        vec![
                                            Note::primary(
                                                span,
                                                "This name does not resolve to a variable...",
                                            ),
                                            Note::secondary(form.span, "...in this file"),
                                        ],
                                    ));

                                    Form::item(span, Item::error(span))
                                }
                            }
                        })
                }
            },
            ParseResult::Apply(lhs, (_, apply), rhs) => (apply.0)(
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

                Form::item(span, Item::error(span))
            }
        }
    }

    fn lower_to_binding(mut self, stack: &Stack, info: &mut Info) -> Option<SpannedBinding> {
        // eventually, support more complex expressions with operators
        if self.items.len() != 1 {
            return None;
        }

        self.items.remove(0).lower_to_binding(stack, info)
    }

    fn lower_to_ty(mut self, stack: &Stack, info: &mut Info) -> Option<Ty> {
        // eventually, support more complex expressions with type parameters
        if self.items.len() != 1 {
            return None;
        }

        self.items.remove(0).lower_to_ty(stack, info)
    }
}

enum ParseResult {
    List(ListExpr),
    Apply(Vec<Expr>, (Span, OperatorApply), Vec<Expr>),
    PartiallyApplyLeft(Vec<Expr>, (Span, OperatorApply)),
    PartiallyApplyRight((Span, OperatorApply), Vec<Expr>),
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
                if let Some(form) = name.resolve(stack, info) {
                    if let FormKind::Operator {
                        precedence,
                        associativity,
                        apply,
                    } = form.kind
                    {
                        operators.push((index, form.span, (precedence, associativity, apply)));
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

            match operator.0.cmp(&max_operator.0) {
                Ordering::Greater => replace!(),
                Ordering::Equal => {
                    if operator.1 != max_operator.1 {
                        return Err(Error::AmbiguousAssociativity {
                            first: self.items[max_index].span(),
                            second: self.items[index].span(),
                        });
                    }

                    match operator.1 {
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
                            return Err(Error::MultipleNonAssociativeOperators {
                                first: self.items[max_index].span(),
                                second: self.items[index].span(),
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
            ParseResult::PartiallyApplyLeft(lhs, (max_span, max_operator.2))
        } else if lhs.is_empty() {
            ParseResult::PartiallyApplyRight((max_span, max_operator.2), rhs)
        } else {
            ParseResult::Apply(lhs, (max_span, max_operator.2), rhs)
        })
    }
}
