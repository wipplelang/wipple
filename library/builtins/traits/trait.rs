use crate::*;

use wipple::*;

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "Trait", Value::of(Trait::of::<Trait>()))?;

    // Trait == Text
    env.add_text_relation::<Trait>("trait", stack)?;

    // Trait == Function
    env.add_relation_between(stack, |r#trait: Trait| {
        Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let trait_value = r#trait.pattern()(&value, env, stack)?
                .ok_or_else(|| error("Cannot use this value to represent this trait", stack))?;

            Ok(Value::new(r#trait.clone(), trait_value.into_owned()))
        })
    })?;

    // Trait == Pattern
    env.add_relation_between(stack, Pattern::for_trait)?;

    Ok(())
}
