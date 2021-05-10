use crate::*;

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Pattern", Value::of(Trait::of::<Pattern>()));

    // Pattern == Text
    env.add_text_relation::<Pattern>("pattern");
}
