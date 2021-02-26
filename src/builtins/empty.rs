use crate::*;

pub(crate) fn setup(env: &mut Environment) {
    // _ : <empty value>
    env.set_variable("_", Value::empty());

    let empty_validation = Validation::new(|value, _, _| {
        Ok(if value.is_empty() {
            Validated::Valid(value.clone())
        } else {
            Validated::Invalid
        })
    });

    // Allow the use of '_' as a catch-all validation that returns its input
    env.add_conformance(TraitID::validation(), {
        let empty_validation = empty_validation.clone();

        move |value, env, stack| empty_validation.0(value, env, stack).map(Option::from)
    });

    // empty : <validation>
    env.set_variable("empty", Value::of(empty_validation));

    // empty ::= Text
    env.add_conformance(TraitID::text(), move |value, _, _| {
        Ok(if value.is_empty() {
            Some(Value::of(Text {
                text: String::from("<empty value>"),
                location: None,
            }))
        } else {
            None
        })
    })
}
