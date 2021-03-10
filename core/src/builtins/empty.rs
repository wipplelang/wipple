use crate::*;

pub(crate) fn setup(env: &mut Environment) {
    // _ : <empty value>
    env.set_variable("_", Value::empty());

    // Allow the use of '_' as a catch-all validation that returns its input
    env.add_conformance(TraitID::validation(), {
        move |value, _, _| {
            if value.is_empty() {
                Ok(Some(Value::of(Validation::any())))
            } else {
                Ok(None)
            }
        }
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

    env.add_conformance(TraitID::text(), move |value, _, _| {
        Ok(if value.is_empty() {
            Some(Value::of(Text::new("<empty value>")))
        } else {
            None
        })
    })
}
