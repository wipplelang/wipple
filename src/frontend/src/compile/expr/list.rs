use crate::{compile::*, *};
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    mem,
};

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
                    LowerContext::Constructor => {
                        Some(Form::constructor(list_expr.span, Constructor::Unit))
                    }
                    _ => todo!(),
                },
                _ => {
                    let mut form = list_expr.items.remove(0).lower(context, stack, info)?;

                    let mut items = list_expr.items.into_iter();
                    while let Some(expr) = items.next() {
                        form = match form.kind {
                            FormKind::Item(item) => {
                                let input = expr.lower_to_item(stack, info);
                                let span = item.info.span.with_end(input.info.span.end);

                                Form::item(span, Item::apply(span, item, input))
                            }
                            FormKind::Template(template) => {
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

                                let span =
                                    form.info.span.with_end(exprs.last().unwrap().span().end);

                                template.expand(context, exprs, span, stack, info)?
                            }
                            FormKind::Operator(operator) => {
                                Form::template(form.info.span, operator.template)
                            }
                            FormKind::Constructor(constructor) => {
                                match context {
                                    LowerContext::Item => match constructor {
                                        Constructor::Placeholder => {
                                            info.diagnostics.add(Diagnostic::new(
                                                DiagnosticLevel::Error,
                                                "Cannot use '_' as a constructor",
                                                vec![Note::primary(
                                                    span,
                                                    "Try specifying a type here",
                                                )],
                                            ));

                                            return None;
                                        }
                                        Constructor::Parameter(var) => {
                                            let name =
                                                var.name.map(|name| name.as_str()).unwrap_or("_");

                                            info.diagnostics.add(Diagnostic::new(
                                                DiagnosticLevel::Error,
                                                format!(
                                                    "Cannot use type '{}' as a constructor",
                                                    name
                                                ),
                                                vec![Note::primary(
                                                    span,
                                                    format!("'{}' is not a concrete type and cannot be used to construct values directly", name),
                                                )],
                                            ));

                                            return None;
                                        }
                                        Constructor::Never
                                        | Constructor::Number
                                        | Constructor::Text
                                        | Constructor::Unit
                                        | Constructor::Function { .. } => {
                                            info.diagnostics.add(Diagnostic::new(
                                                DiagnosticLevel::Error,
                                                "Cannot use primitive type as a constructor",
                                                vec![Note::primary(
                                                    span,
                                                    "Try using a literal instead",
                                                )],
                                            ));

                                            return None;
                                        }
                                        Constructor::DataStruct {
                                            id,
                                            fields: ref constructor_fields,
                                        } => {
                                            let block_span = expr.span();

                                            let block = match expr {
                                                Expr::Block(block) => block,
                                                _ => {
                                                    info.diagnostics.add(Diagnostic::new(
                                                        DiagnosticLevel::Error,
                                                        "Expected block in constructor",
                                                        vec![Note::primary(
                                                            expr.span(),
                                                            "Expected block here",
                                                        )],
                                                    ));

                                                    return None;
                                                }
                                            };

                                            let mut fields =
                                                HashMap::<InternedString, DataStructField>::new();
                                            let mut success = true;
                                            for statement in block.statements {
                                                let field = match statement
                                                    .lower_to_data_struct_field(stack, info)
                                                {
                                                    Some(field) => field,
                                                    None => {
                                                        success = false;
                                                        continue;
                                                    }
                                                };

                                                match fields.entry(field.name) {
                                                    Entry::Occupied(entry) => {
                                                        let existing_field = entry.get();

                                                        info.diagnostics.add(Diagnostic::new(
                                                            DiagnosticLevel::Error,
                                                            "Duplicate data structure field",
                                                            vec![
                                                                Note::primary(
                                                                    field.info.span,
                                                                    "Field already exists",
                                                                ),
                                                                Note::secondary(
                                                                    existing_field.info.span,
                                                                    "Existing field declared here",
                                                                ),
                                                            ],
                                                        ));

                                                        success = false;
                                                    }
                                                    Entry::Vacant(entry) => {
                                                        entry.insert(field);
                                                    }
                                                }
                                            }

                                            if !success {
                                                return None;
                                            }

                                            let missing_fields = constructor_fields
                                                .keys()
                                                .filter(|name| !fields.contains_key(*name))
                                                .map(|name| name.to_string())
                                                .collect::<Vec<_>>();

                                            if !missing_fields.is_empty() {
                                                info.diagnostics.add(Diagnostic::new(
                                                    DiagnosticLevel::Error,
                                                    format!(
                                                        "Missing fields in data structure: {}",
                                                        missing_fields.join(", ")
                                                    ),
                                                    vec![Note::primary(
                                                        block_span,
                                                        "Add the missing fields here",
                                                    )],
                                                ));

                                                return None;
                                            }

                                            let mut fields_by_index = Vec::new();
                                            let mut success = true;
                                            for (field_name, field) in fields {
                                                match constructor_fields
                                                    .keys()
                                                    .enumerate()
                                                    .find_map(|(index, &name)| {
                                                        (name == field_name).then(|| index)
                                                    }) {
                                                    Some(index) => {
                                                        fields_by_index.push((index, field));
                                                    }
                                                    None => {
                                                        info.diagnostics.add(Diagnostic::new(
                                                            DiagnosticLevel::Error,
                                                            format!("Data structure does not contain field '{}'", field_name),
                                                            vec![
                                                                Note::primary(
                                                                    field.info.span,
                                                                    "No such field",
                                                                ),
                                                            ],
                                                        ));

                                                        success = false;
                                                    }
                                                }
                                            }

                                            if !success {
                                                return None;
                                            }

                                            fields_by_index.sort_by_key(|(index, _)| *index);

                                            let fields = fields_by_index
                                                .into_iter()
                                                .map(|(_, field)| field.value)
                                                .collect::<Vec<_>>();

                                            Form::item(span, Item::data(span, id, fields))
                                        }
                                    },
                                    LowerContext::Constructor => todo!(), // generic arguments
                                    LowerContext::File
                                    | LowerContext::Template
                                    | LowerContext::Operator
                                    | LowerContext::Binding
                                    | LowerContext::DataStructField
                                    | LowerContext::DataStructFieldDecl => unreachable!(),
                                }
                            }
                            FormKind::File(file) => {
                                let span = expr.span();

                                let name = match expr {
                                    Expr::Name(name) => name,
                                    _ => {
                                        info.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "Expected name",
                                            vec![Note::primary(
                                                form.info.span,
                                                "Expected variable name here",
                                            )],
                                        ));

                                        return None;
                                    }
                                };

                                let variable = file.variables.get(&name.value).cloned();

                                if let Some(variable) = variable {
                                    let form = variable.form(span, context, info)?;

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
                                            Note::secondary(form.info.span, "...in this file"),
                                        ],
                                    ));

                                    return None;
                                }
                            }
                            FormKind::Binding(_) => todo!(), // TODO: Complex bindings
                            FormKind::DataStructField(_) | FormKind::DataStructFieldDecl(_) => {
                                unreachable!()
                            }
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
                // NOTE: This relies on the fact that form won't emit diagnostics if they aren't
                // operators -- otherwise we'd get an error for most forms in the list
                if let Some(Some(form)) = name.resolve(LowerContext::Operator, stack, info) {
                    if let FormKind::Operator(operator) = form.kind {
                        operators.push((index, form.info.span, operator));
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
                    if operator.precedence.associativity()
                        != max_operator.precedence.associativity()
                    {
                        return Err(Error::AmbiguousAssociativity {
                            first: self.items[max_index].span(),
                            second: self.items[index].span(),
                        });
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
                            return Err(Error::MultipleNonAssociativeOperators {
                                first: self.items[max_index].span(),
                                second: self.items[index].span(),
                            });
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
