use crate::*;

core_primitive!(pub validation for Validation);

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Validation", Value::of(Trait::of::<Validation>()));

    // Validation == Text
    env.add_text_conformance(Trait::validation(), "validation");
}
