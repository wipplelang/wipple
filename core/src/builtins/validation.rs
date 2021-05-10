use crate::*;

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Validation", Value::of(Trait::of::<Validation>()));

    // Validation == Text
    env.add_text_conformance::<Validation>("validation");
}
