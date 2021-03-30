use crate::*;

#[derive(Clone, Copy)]
struct Empty;

fundamental_primitive!(pub empty for Empty);

impl Value {
    pub fn empty() -> Self {
        Value::of(Empty)
    }

    pub fn is_empty(&self) -> bool {
        self.r#trait == Trait::of::<Empty>()
    }
}

pub(crate) fn setup(env: &mut Environment) {
    // _ : <empty value>
    env.set_variable("_", Value::empty());

    // Allow the use of '_' as a catch-all validation that returns its input
    env.add_conformance(Trait::empty(), Trait::validation(), move |_, _, _| {
        Ok(Value::of(Validation::any()))
    });

    // empty : <validation>
    env.set_variable(
        "empty",
        Value::of(Validation::new(|value, _, _| {
            Ok(if value.is_empty() {
                Validated::Valid(value.clone())
            } else {
                Validated::Invalid
            })
        })),
    );

    // empty ::= Text
    env.add_conformance(Trait::empty(), Trait::text(), move |_, _, _| {
        Ok(Value::of(Text::new("<empty value>")))
    })
}
