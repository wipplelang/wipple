use crate::{lower::*, typecheck::Ty};
use std::{cmp::Ordering, mem};

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

    fn lower(self, context: LowerContext, stack: &Stack, info: &mut Info) -> Option<Form> {
        let span = self.span;

        let form = match self.parse_operators(stack, info) {
            Ok(operators) => operators,
            Err(error) => {
                info.diagnostics.add(error.into());
                return None;
            }
        };

        match form {
            ParseResult::List(mut list_expr) => match list_expr.items.len() {
                0 => match context {
                    LowerContext::Item => {
                        Some(Form::item(list_expr.span, Item::unit(list_expr.span)))
                    }
                    LowerContext::Ty => Some(Form::ty(list_expr.span, Ty::unit())),
                    _ => todo!(),
                }, // TODO: empty tuple type
                _ => {
                    let mut form = list_expr.items.remove(0).lower(context, stack, info)?;

                    let mut items = list_expr.items.into_iter();
                    while let Some(expr) = items.next() {
                        form = match form.kind {
                            FormKind::Item { item } => {
                                let input = expr.lower_to_item(stack, info)?;
                                let span = item.debug_info.span.with_end(input.debug_info.span.end);

                                Form::item(span, Item::apply(span, Box::new(item), Box::new(input)))
                            }
                            FormKind::Template { template } => {
                                let mut exprs = vec![expr];

                                if let Some(arity) = template.arity {
                                    let arity = usize::from(arity);

                                    for _ in 1..arity {
                                        let expr = match items.next() {
                                            Some(expr) => expr,
                                            None => break,
                                        };

                                        exprs.push(expr);
                                    }

                                    if exprs.len() < arity {
                                        info.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            format!(
                                                "This template accepts {} expressions, but only {} were given",
                                                arity,
                                                exprs.len(),
                                            ),
                                            vec![
                                                Note::primary(
                                                    span,
                                                    format!(
                                                        "Expected {} more values here",
                                                        arity - exprs.len()
                                                    ),
                                                ),
                                            ],
                                        ));

                                        return None;
                                    }
                                } else {
                                    exprs.extend(mem::replace(&mut items, Vec::new().into_iter()))
                                }

                                let span = form.span.with_end(exprs.last().unwrap().span().end);

                                template.expand(context, exprs, span, stack, info)?
                            }
                            FormKind::Operator { operator } => {
                                Form::template(form.span, operator.template)
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

                                        return None;
                                    }
                                };

                                if let Some(variable) = file.variables.get(&name.value) {
                                    let form = (variable.form)(span);

                                    if matches!(form.kind, FormKind::Item { .. }) {
                                        info.used_variables.borrow_mut().insert(variable.id);
                                    }

                                    form
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

                                    return None;
                                }
                            }
                            FormKind::Binding { .. } => todo!(), // TODO: Complex bindings
                        };
                    }

                    Some(form)
                }
            },
            ParseResult::Apply(lhs, (_, operator), rhs) => operator.template.expand(
                context,
                vec![
                    Expr::List(ListExpr::infer_span(lhs)),
                    Expr::List(ListExpr::infer_span(rhs)),
                ],
                span,
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

                None
            }
        }
    }
}

enum ParseResult {
    List(ListExpr),
    Apply(Vec<Expr>, (Span, Operator), Vec<Expr>),
    PartiallyApplyLeft(Vec<Expr>, (Span, Operator)),
    PartiallyApplyRight((Span, Operator), Vec<Expr>),
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
                    if let FormKind::Operator { operator } = form.kind {
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
                            first: self.items[max_index].span(),
                            second: self.items[index].span(),
                        });
                    }

                    match operator.associativity {
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
            ParseResult::PartiallyApplyLeft(lhs, (max_span, max_operator))
        } else if lhs.is_empty() {
            ParseResult::PartiallyApplyRight((max_span, max_operator), rhs)
        } else {
            ParseResult::Apply(lhs, (max_span, max_operator), rhs)
        })
    }
}
