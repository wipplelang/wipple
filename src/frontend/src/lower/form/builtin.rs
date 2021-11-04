use crate::{debug_info::DebugInfo, lower::*, typecheck::Ty};
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
            OperatorPrecedence::new(9),
            OperatorAssociativity::None,
            OperatorApply::new(move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;
                let span = lhs_span.with_end(rhs_span.end);

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

                        return Form::item(span, Item::error(span));
                    }
                };

                Form::item(span, binding.assign_in_block(span, value, stack, info))
            }),
        )
    }

    fn builtin_annotate(span: Span) -> Self {
        Form::operator(
            span,
            OperatorPrecedence::new(8),
            OperatorAssociativity::Left,
            OperatorApply::new(move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;
                let span = lhs_span.with_end(rhs_span.end);

                let value = lhs.lower_to_item(stack, info);

                let ty = match rhs.lower_to_ty(stack, info) {
                    Some(ty) => ty,
                    None => return Form::item(span, Item::error(span)),
                };

                Form::item(span, Item::annotate(span, Box::new(value), ty))
            }),
        )
    }

    fn builtin_function(span: Span) -> Self {
        Form::operator(
            span,
            OperatorPrecedence::new(7),
            OperatorAssociativity::Right,
            OperatorApply::new(move |lhs, rhs, stack, info| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;
                let span = lhs_span.with_end(rhs_span.end);

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

                        return Form::item(span, Item::error(span));
                    }
                };

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
                        rhs.lower_to_item(&stack, info),
                    ],
                );

                let captures = Rc::try_unwrap(stack.captures.unwrap())
                    .unwrap_or_else(|_| unreachable!())
                    .into_inner();

                Form::item(
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
                )
            }),
        )
    }

    fn builtin_external(span: Span) -> Self {
        todo!()
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
