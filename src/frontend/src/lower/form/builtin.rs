use crate::lower::*;
use std::collections::HashMap;
use wipple_diagnostics::Span;
use wipple_parser::Intern;

pub fn builtins() -> HashMap<Intern<String>, Variable> {
    macro_rules! builtins {
        ($($name:expr => $value:expr,)*) => {{
            let mut variables = HashMap::default();

            $({
                let name = Intern::from($name);

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
        "external" => SpannedForm::builtin_external,
    }
}

impl SpannedForm {
    fn builtin_assign(span: Span) -> Self {
        SpannedForm::operator(
            span,
            OperatorPrecedence::new(9),
            OperatorAssociativity::None,
            move |lhs, rhs, stack, diagnostics| {
                let lhs_span = lhs.span;
                let rhs_span = rhs.span;

                let binding = lhs.lower_to_binding(stack, diagnostics);
                let value = rhs.lower_to_form(stack, diagnostics);

                binding
                    .assign(lhs_span.with_end(rhs_span.end), value, stack, diagnostics)
                    .into()
            },
        )
    }
}

impl SpannedForm {
    fn builtin_external(span: Span) -> Self {
        todo!()
    }
}
