use crate::{debug_info::DebugInfo, lower::*, typecheck::Ty};
use std::{collections::HashMap, num::NonZeroUsize, rc::Rc};
use wipple_diagnostics::*;

pub fn builtins() -> HashMap<LocalIntern<String>, Variable> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = LocalIntern::from($name);

                variables.insert(
                    name,
                    Variable::compile_time(Span::default(), name, $value),
                );
            })*

            variables
        }};
    }

    builtins! {
        ":" => Form::builtin_assign,
        "::" => Form::builtin_annotate,
        "->" => Form::builtin_function,
        "external" => Form::builtin_external,
        "file" => Form::builtin_file,
        "use" => Form::builtin_use,
        "Number" => Form::builtin_number_ty,
        "Text" => Form::builtin_text_ty,
    }
}

impl Form {
    fn builtin_assign(span: Span) -> Self {
        Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(9),
                associativity: OperatorAssociativity::None,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |_, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();
                        let lhs = exprs.next().unwrap();
                        let rhs = exprs.next().unwrap();

                        let binding = lhs.lower_to_binding(stack, info)?;
                        let value = rhs.lower(LowerContext::Item, stack, info)?;

                        Some(Form::item(
                            span,
                            binding.assign_in_block(span, value, stack, info),
                        ))
                    },
                ),
            },
        )
    }

    fn builtin_annotate(span: Span) -> Self {
        Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(8),
                associativity: OperatorAssociativity::Left,
                template: Template::new(
                    Some(NonZeroUsize::new(2).unwrap()),
                    move |_, exprs, span, stack, info| {
                        let mut exprs = exprs.into_iter();
                        let lhs = exprs.next().unwrap();
                        let rhs = exprs.next().unwrap();

                        let value = lhs.lower_to_item(stack, info)?;
                        let ty = rhs.lower_to_ty(stack, info)?;

                        Some(Form::item(span, Item::annotate(span, Box::new(value), ty)))
                    },
                ),
            },
        )
    }

    fn builtin_function(span: Span) -> Self {
        Form::operator(
            span,
            Operator {
                precedence: OperatorPrecedence::new(7),
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
                            LowerContext::Ty => {
                                let input_ty = lhs.lower_to_ty(stack, info)?;
                                let body_ty = rhs.lower_to_ty(stack, info)?;

                                Some(Form::ty(span, Ty::function(input_ty, body_ty)))
                            }
                            LowerContext::Item => {
                                let binding = lhs.lower_to_binding(stack, info)?;
                                let binding_span = binding.span();

                                let stack = stack.child_function();

                                let body = Item::block(
                                    rhs_span,
                                    vec![
                                        binding.assign_in_block(
                                            lhs_span,
                                            Form::item(lhs_span, Item::function_input(lhs_span)),
                                            &stack,
                                            info,
                                        ),
                                        rhs.lower_to_item(&stack, info)?,
                                    ],
                                );

                                let captures = Rc::try_unwrap(stack.captures.unwrap())
                                    .unwrap_or_else(|_| unreachable!())
                                    .into_inner();

                                Some(Form::item(
                                    span,
                                    Item::function(
                                        span,
                                        DebugInfo {
                                            declared_name: None,
                                            span: binding_span,
                                        },
                                        Box::new(body),
                                        captures,
                                    ),
                                ))
                            }
                            _ => None,
                        }
                    },
                ),
            },
        )
    }

    fn builtin_external(span: Span) -> Self {
        Form::template(
            span,
            Template::new(
                Some(NonZeroUsize::new(2).unwrap()),
                move |_, exprs, span, _, info| {
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

                    Some(Form::item(
                        span,
                        Item::external(span, namespace, identifier),
                    ))
                },
            ),
        )
    }

    fn builtin_file(span: Span) -> Self {
        todo!()
    }

    fn builtin_use(span: Span) -> Self {
        todo!()
    }

    fn builtin_number_ty(span: Span) -> Self {
        Form::ty(span, Ty::number())
    }

    fn builtin_text_ty(span: Span) -> Self {
        Form::ty(span, Ty::text())
    }
}
