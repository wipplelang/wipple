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
    env.add_conformance(
        ID::validation(),
        empty_validation.clone(),
        move |_, _, _| Ok(Value::of(Validation::any())),
    );

    // empty : <validation>
    env.set_variable("empty", Value::of(empty_validation.clone()));

    // empty ::= Text
    env.add_conformance(ID::text(), empty_validation, move |_, _, _| {
        Ok(Value::of(Text::new("<empty value>")))
    })
}
