use crate::{compile::*, *};
use std::{
    collections::{btree_map::Entry, BTreeMap, HashMap},
    num::NonZeroUsize,
};
use wipple_diagnostics::*;

pub fn builtin_variables() -> HashMap<InternedString, Variable> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = InternedString::new($name);
                variables.insert(name, Variable::compile_time($value));
            })*

            variables
        }};
    }

    builtins! {
        "_" => Form::builtin_underscore,
        ":" => Form::builtin_assign,
        "::" => Form::builtin_annotate,

        "->" => Form::builtin_function,
        "return" => Form::builtin_return,

        ".." => Form::builtin_field,

        "external" => Form::builtin_external,
        "file" => Form::builtin_file,
        "use" => Form::builtin_use,

        "data" => Form::builtin_data,

        "loop" => Form::builtin_loop,
        "end" => Form::builtin_end,

        "Number" => Form::builtin_number_ty,
        "Text" => Form::builtin_text_ty,
    }
}

impl Form {
    fn builtin_underscore(span: Span, context: LowerContext, info: &mut Info) -> Option<Self> {
        match context {
            LowerContext::Item => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected value, found '_'",
                    vec![Note::primary(span, "Expected a value here")],
                ));

                None
            }
            LowerContext::Operator => None,
            LowerContext::Template => None,
            LowerContext::Constructor => Some(Form::constructor(span, Constructor::Placeholder)),
            LowerContext::File => None,
            LowerContext::Binding => {
                // TODO: Empty bindings
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "'_' in bindings is currently unsupported",
                    vec![Note::primary(span, "Try assigning to a name instead")],
                ));

                None
            }
            LowerContext::DataStructField => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected data structure field, found '_'",
                    vec![Note::primary(span, "Expected a data structure field here")],
                ));

                None
            }
            LowerContext::DataStructFieldDecl => {
                info.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Expected data structure field declaration, found '_'",
                    vec![Note::primary(
                        span,
                        "Expected a data structure field declaration here",
                    )],
                ));

                None
            }
        }
    }

    fn builtin_assign(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(9),
                associativity: OperatorAssociativity::None,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |context, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();
                        let lhs = exprs.next().unwrap();
                        let rhs = exprs.next().unwrap();

                        match context {
                            LowerContext::Item => {
                                let binding = lhs.lower_to_binding(stack, info)?;
                                let value = rhs.lower(LowerContext::Item, stack, info)?;

                                Some(Form::item(span, binding.assign(span, value, stack, info)))
                            }
                            LowerContext::DataStructField => {
                                let lhs_span = lhs.span();

                                let lhs = match lhs {
                                    Expr::List(list) => list.items,
                                    _ => unreachable!(),
                                };

                                let name = match lhs.len() {
                                    1 => match lhs.into_iter().next().unwrap() {
                                        Expr::Name(name) => name.value,
                                        lhs => {
                                            info.diagnostics.add(Diagnostic::new(
                                                DiagnosticLevel::Error,
                                                "Expected name for data structure field",
                                                vec![Note::primary(
                                                    lhs.span(),
                                                    "Expected name here",
                                                )],
                                            ));

                                            return None;
                                        }
                                    },
                                    _ => {
                                        info.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "Expected name for data structure field",
                                            vec![Note::primary(lhs_span, "Complex bindings are not supported; you must declare one field at a time")],
                                        ));

                                        return None;
                                    }
                                };

                                let value = rhs.lower_to_item(stack, info)?;

                                Some(Form::data_struct_field(
                                    span,
                                    DataStructField::new(
                                        DataStructFieldInfo::new(span),
                                        name,
                                        value,
                                    ),
                                ))
                            }
                            LowerContext::DataStructFieldDecl
                            | LowerContext::Constructor
                            | LowerContext::Binding => {
                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Cannot assign to variable here",
                                    vec![Note::primary(
                                        span,
                                        "Assignment is disallowed in this context",
                                    )],
                                ));

                                None
                            }
                            LowerContext::Operator
                            | LowerContext::Template
                            | LowerContext::File => unreachable!(),
                        }
                    },
                ),
            },
        ))
    }

    fn builtin_annotate(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(0),
                associativity: OperatorAssociativity::Left,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |context, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();
                        let lhs = exprs.next().unwrap();
                        let rhs = exprs.next().unwrap();

                        match context {
                            LowerContext::Item => {
                                let value = lhs.lower_to_item(stack, info)?;
                                let constructor = rhs.lower_to_constructor(stack, info)?;

                                Some(Form::item(span, Item::annotate(span, value, constructor)))
                            }
                            LowerContext::DataStructFieldDecl => {
                                let lhs_span = lhs.span();

                                let lhs = match lhs {
                                    Expr::List(list) => list.items,
                                    _ => unreachable!(),
                                };

                                let name = match lhs.len() {
                                    1 => match lhs.into_iter().next().unwrap() {
                                        Expr::Name(name) => name.value,
                                        lhs => {
                                            info.diagnostics.add(Diagnostic::new(
                                                DiagnosticLevel::Error,
                                                "Expected name for data structure field",
                                                vec![Note::primary(
                                                    lhs.span(),
                                                    "Expected name here",
                                                )],
                                            ));

                                            return None;
                                        }
                                    },
                                    _ => {
                                        info.diagnostics.add(Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            "Expected name for data structure field",
                                            vec![Note::primary(lhs_span, "Complex bindings are not supported; you must declare one field at a time")],
                                        ));

                                        return None;
                                    }
                                };

                                let ty = rhs.lower_to_constructor(stack, info)?;

                                Some(Form::data_struct_field_decl(
                                    span,
                                    DataStructFieldDecl::new(
                                        DataStructFieldDeclInfo::new(span, name),
                                        ty,
                                    ),
                                ))
                            }
                            LowerContext::Binding => {
                                // TODO: Bindings with type annotations
                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Type annotations in bindings are currently unsupported",
                                    vec![Note::primary(
                                        span,
                                        "Add the type annotation to the value instead",
                                    )],
                                ));

                                None
                            }
                            LowerContext::DataStructField | LowerContext::Constructor => {
                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Cannot add type annotation here",
                                    vec![Note::primary(
                                        span,
                                        "Type annotations are disallowed in this context",
                                    )],
                                ));

                                None
                            }
                            LowerContext::Operator
                            | LowerContext::Template
                            | LowerContext::File => unreachable!(),
                        }
                    },
                ),
            },
        ))
    }

    fn builtin_function(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(8),
                associativity: OperatorAssociativity::Right,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |context, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();

                        let lhs = exprs.next().unwrap();
                        let lhs_span = lhs.span();

                        let rhs = exprs.next().unwrap();
                        let rhs_span = rhs.span();

                        match context {
                            LowerContext::Constructor => {
                                let input_ty = lhs.lower_to_constructor(stack, info)?;
                                let body_ty = rhs.lower_to_constructor(stack, info)?;

                                Some(Form::constructor(
                                    span,
                                    Constructor::Function {
                                        input: Box::new(input_ty),
                                        output: Box::new(body_ty),
                                    },
                                ))
                            }
                            LowerContext::Item => {
                                let binding = lhs.lower_to_binding(stack, info)?;

                                let stack = stack.child_function();

                                let body = Item::block(
                                    rhs_span,
                                    vec![
                                        binding.assign(
                                            lhs_span,
                                            Form::item(lhs_span, Item::function_input(lhs_span)),
                                            &stack,
                                            info,
                                        ),
                                        rhs.lower_to_item(&stack, info)?,
                                    ],
                                );

                                let captures = Arc::try_unwrap(stack.captures.unwrap())
                                    .unwrap_or_else(|_| unreachable!())
                                    .into_inner();

                                Some(Form::item(span, Item::function(span, body, captures)))
                            }
                            _ => None,
                        }
                    },
                ),
            },
        ))
    }

    fn builtin_return(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, stack, info| {
                    let item = exprs
                        .into_iter()
                        .next()
                        .unwrap()
                        .lower_to_item(stack, info)?;

                    Some(Form::item(span, Item::r#return(span, item)))
                },
            ),
        ))
    }

    fn builtin_field(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(7),
                associativity: OperatorAssociativity::Left,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |_, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();

                        let item = exprs.next().unwrap().lower_to_item(stack, info)?;

                        let mut rhs = match exprs.next().unwrap() {
                            Expr::List(list) => list,
                            _ => unreachable!(),
                        };

                        let diagnostic = || {
                            Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Expected name of data structure field",
                                vec![Note::primary(rhs.span, "Expected name here")],
                            )
                        };

                        if rhs.items.len() != 1 {
                            info.diagnostics.add(diagnostic());
                            return None;
                        }

                        let field = match rhs.items.remove(0) {
                            Expr::Name(name) => name,
                            _ => {
                                info.diagnostics.add(diagnostic());
                                return None;
                            }
                        };

                        Some(Form::item(
                            span,
                            Item::field(span, item, field.span, field.value),
                        ))
                    },
                ),
            },
        ))
    }

    fn builtin_external(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(None, move |_, exprs, span, stack, info| {
                let mut exprs = exprs.into_iter();

                let namespace = match exprs.next().unwrap() {
                    Expr::Text(namespace) => namespace.value,
                    expr => {
                        info.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Expected namespace",
                            vec![Note::primary(expr.span(), "Expected a text value here")],
                        ));

                        return None;
                    }
                };

                let identifier = match exprs.next().unwrap() {
                    Expr::Text(identifier) => identifier.value,
                    expr => {
                        info.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Expected identifier",
                            vec![Note::primary(expr.span(), "Expected a text value here")],
                        ));

                        return None;
                    }
                };

                let inputs = exprs
                    .map(|expr| expr.lower_to_item(stack, info))
                    .collect::<Option<Vec<_>>>()?;

                Some(Form::item(
                    span,
                    Item::external(span, namespace, identifier, inputs),
                ))
            }),
        ))
    }

    fn builtin_file(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, _, info| {
                    let mut exprs = exprs.into_iter();

                    let file = match exprs.next().unwrap() {
                        Expr::Text(text) => text.value,
                        expr => {
                            info.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Expected path or URL to file",
                                vec![Note::primary(expr.span(), "Expected a text value here")],
                            ));

                            return None;
                        }
                    };

                    let file = project::load_file(&file, span, info)?;

                    Some(Form::file(span, file))
                },
            ),
        ))
    }

    fn builtin_use(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, stack, info| {
                    let mut exprs = exprs.into_iter();

                    let file = match exprs.next().unwrap() {
                        Expr::Text(text) => text.value,
                        expr => {
                            info.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Expected path or URL to file",
                                vec![Note::primary(expr.span(), "Expected a text value here")],
                            ));

                            return None;
                        }
                    };

                    let file = project::load_file(&file, span, info)?;

                    let mut variables = stack.variables.borrow_mut();
                    for (name, variable) in &file.variables {
                        variables.entry(*name).or_insert_with(|| variable.clone());
                    }

                    Some(Form::item(span, Item::unit(span)))
                },
            ),
        ))
    }

    fn builtin_data(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, stack, info| {
                    let mut exprs = exprs.into_iter();

                    // TODO: Tuple data structures
                    let block = match exprs.next().unwrap() {
                        Expr::Block(block) => block,
                        expr => {
                            info.diagnostics.add(Diagnostic::new(
                                DiagnosticLevel::Error,
                                "Expected block",
                                vec![Note::primary(
                                    expr.span(),
                                    "Tuple data structures are not currently supported",
                                )],
                            ));

                            return None;
                        }
                    };

                    let mut fields = BTreeMap::<InternedString, DataStructFieldDecl>::new();
                    let mut success = true;
                    for statement in block.statements {
                        let field = match statement.lower_to_data_struct_field_decl(stack, info) {
                            Some(field) => field,
                            None => {
                                success = false;
                                continue;
                            }
                        };

                        match fields.entry(field.info.name) {
                            Entry::Occupied(entry) => {
                                let existing_field = entry.get();

                                info.diagnostics.add(Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    "Duplicate data structure field declaration",
                                    vec![
                                        Note::primary(field.info.span, "Field already exists"),
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

                    success.then(|| {
                        Form::constructor(
                            span,
                            Constructor::DataStruct {
                                id: TypeId::new(),
                                fields,
                            },
                        )
                    })
                },
            ),
        ))
    }

    fn builtin_loop(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, stack, info| {
                    let item = exprs
                        .into_iter()
                        .next()
                        .unwrap()
                        .lower_to_item(stack, info)?;

                    Some(Form::item(span, Item::r#loop(span, item)))
                },
            ),
        ))
    }

    fn builtin_end(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(1).unwrap()),
                move |_, exprs, span, stack, info| {
                    let item = exprs
                        .into_iter()
                        .next()
                        .unwrap()
                        .lower_to_item(stack, info)?;

                    Some(Form::item(span, Item::end(span, item)))
                },
            ),
        ))
    }

    fn builtin_number_ty(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::constructor(span, Constructor::Number))
    }

    fn builtin_text_ty(span: Span, _: LowerContext, _: &mut Info) -> Option<Self> {
        Some(Form::constructor(span, Constructor::Text))
    }
}
