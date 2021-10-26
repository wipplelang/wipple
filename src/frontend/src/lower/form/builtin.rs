use crate::lower::*;
use std::{collections::HashMap, rc::Rc};
use wipple_diagnostics::*;

pub fn builtins() -> HashMap<LocalIntern<String>, Variable> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = LocalIntern::from($name);

                variables.insert(
                    name,
                    Variable::compiletime(Span::default(), name, $value),
                );
            })*

            variables
        }};
    }

    builtins! {
        ":" => Form::builtin_assign,
        "->" => Form::builtin_function,
        "external" => Form::builtin_external,
    }
}

impl Form {
    fn builtin_assign(span: Span) -> Self {
        Form::operator(
            span,
            OperatorPrecedence::new(9),
            OperatorAssociativity::None,
            move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;

                let binding = lhs.lower_to_binding(stack, info);
                let value = rhs.lower_to_form(stack, info);

                let binding = match binding {
                    Some(binding) => binding,
                    None => {
                        info.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Cannot assign to this",
                            vec![Note::primary(
                                lhs_span,
                                "Expected a variable name or other assignable expression",
                            )],
                        ));

                        return Form::Item(Item::error(span));
                    }
                };

                Form::Item(binding.assign(lhs_span.with_end(rhs_span.end), value, stack, info))
            },
        )
    }

    fn builtin_function(span: Span) -> Self {
        Form::operator(
            span,
            OperatorPrecedence::new(8),
            OperatorAssociativity::Right,
            move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;

                let binding = match lhs.lower_to_binding(stack, info) {
                    Some(binding) => binding,
                    None => {
                        info.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Invalid function definition",
                            vec![Note::primary(
                                lhs_span,
                                "Expected this to be a parameter name or other assignable expression",
                            )],
                        ));

                        return Form::Item(Item::error(span));
                    }
                };

                let binding_span = binding.span();

                let stack = stack.child_function();

                let body = Item::block(
                    rhs_span,
                    vec![
                        binding.assign(
                            lhs_span,
                            Form::Item(Item::function_input(lhs_span)),
                            &stack,
                            info,
                        ),
                        rhs.lower_to_item(&stack, info),
                    ],
                );

                let captures = Rc::try_unwrap(stack.captures.unwrap())
                    .unwrap_or_else(|_| unreachable!())
                    .into_inner();

                Form::Item(Item::function(span, binding_span, body, captures))
            },
        )
    }

    fn builtin_external(span: Span) -> Self {
        todo!()
    }
}
