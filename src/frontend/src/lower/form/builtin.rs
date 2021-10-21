use crate::lower::*;
use std::collections::HashMap;
use wipple_diagnostics::*;

pub fn builtins() -> HashMap<LocalIntern<String>, Variable> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = LocalIntern::from($name);

                variables.insert(
                    name,
                    Variable::new(Span::default(), name, $value),
                );
            })*

            variables
        }};
    }

    builtins! {
        ":" => SpannedForm::builtin_assign,
        "->" => SpannedForm::builtin_function,
        "external" => SpannedForm::builtin_external,
    }
}

impl SpannedForm {
    fn builtin_assign(span: Span) -> Self {
        SpannedForm::operator(
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

                        return SpannedItem::error(span).into();
                    }
                };

                binding
                    .assign(lhs_span.with_end(rhs_span.end), value, stack, info)
                    .into()
            },
        )
    }

    fn builtin_function(span: Span) -> Self {
        SpannedForm::operator(
            span,
            OperatorPrecedence::new(8),
            OperatorAssociativity::None,
            move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;

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

                        return SpannedItem::error(span).into();
                    }
                };

                let stack = stack.child_function();

                let body = SpannedItem::block(
                    rhs.span,
                    vec![
                        binding.assign(
                            lhs_span,
                            SpannedItem::function_input(lhs_span).into(),
                            &stack,
                            info,
                        ),
                        rhs.lower_to_item(&stack, info),
                    ],
                );

                let function = Function::new(body, stack.captures.unwrap().into_inner());
                let function_id = function.id;

                info.functions.insert(function_id, function);

                SpannedItem::function(span, function_id).into()
            },
        )
    }

    fn builtin_external(span: Span) -> Self {
        todo!()
    }
}
