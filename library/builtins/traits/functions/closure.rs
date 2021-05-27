use crate::*;
use wipple::*;

#[derive(TypeInfo, Clone)]
pub struct Closure {
    pub captured_env: Env,
    pub parameter_pattern: Pattern,
    pub parameter_name: String,
    pub return_value: Value,
}

impl Primitive for Closure {}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    // Closure == Function
    env.add_relation_between(stack, |closure: Closure| {
        Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let validated = (closure.parameter_pattern)(&value, env, stack)?;

            let value = validated
                .ok_or_else(|| error("Cannot use this value as input to this closure", stack))?;

            let inner_env = closure.captured_env.child();

            inner_env.set_variable(&closure.parameter_name, value.into_owned());

            let result = closure.return_value.evaluate(&inner_env, stack)?;

            Ok(result.into_owned())
        })
    })?;

    // Closure == Text
    env.add_text_relation::<Closure>("closure", stack)?;

    Ok(())
}
