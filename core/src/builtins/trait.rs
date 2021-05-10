use crate::*;

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Trait", Value::of(Trait::of::<Trait>()));

    // Trait == Text
    env.add_text_conformance::<Trait>("trait");

    // Trait == Function
    env.add_primitive_conformance(|r#trait: Trait| {
        Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let trait_value = (r#trait.pattern)(value, env, stack)?
                .into_valid()
                .ok_or_else(|| {
                    Return::error("Cannot use this value to represent this trait", stack)
                })?;

            Ok(Value::new(r#trait.clone(), trait_value))
        })
    });

    // Trait == pattern
    env.add_primitive_conformance(Pattern::for_trait);
}
