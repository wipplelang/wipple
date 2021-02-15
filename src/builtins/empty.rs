use crate::builtins::*;
use crate::fundamentals::*;

pub(crate) fn init(env: &mut Environment) {
    // _ : <empty value>
    env.variables.insert(String::from("_"), Value::empty());

    let empty_validation = Validation::new(|value: Value, _, _| {
        Ok(if value.traits.is_empty() {
            Valid(value.clone())
        } else {
            Invalid
        })
    });

    // Allow the use of '_' as a catch-all validation that returns its input
    env.add_conformance(Conformance::new(
        TraitID::validation_container,
        empty_validation.clone(),
        |_, _, _| {
            Ok(Validation::new(|value: Value, _, _| {
                Ok(Valid(value.clone()))
            }))
        },
    ));

    // empty : validation
    env.variables.insert(
        String::from("empty"),
        Value::new(Trait::validation_container(empty_validation.clone())),
    );

    // empty ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        empty_validation,
        |_, _, _| Ok(Text(String::from("<empty value>"))),
    ));
}
