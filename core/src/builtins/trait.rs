use crate::*;

core_primitive!(pub r#trait for Trait);

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Trait", Value::of(Trait::of::<Trait>()));

    // Trait == Text
    env.add_text_conformance(Trait::r#trait(), "trait");

    // Trait == Function
    env.add_primitive_conformance(|r#trait: Trait| {
        Function::new(move |value, env, stack| {
            Value::new_validated(
                r#trait.clone(),
                value.evaluate(env, stack.clone())?,
                env,
                stack,
            )
        })
    });

    // Trait == Validation
    env.add_primitive_conformance(Validation::for_trait)
}
