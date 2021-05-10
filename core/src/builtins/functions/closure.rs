use crate::*;

#[derive(TypeInfo, Clone)]
pub struct Closure {
    pub captured_env: Environment,
    pub pattern: Pattern,
    pub parameter: Name,
    pub return_value: Value,
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    // Closure == Function
    env.add_primitive_relation(|closure: Closure| {
        Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let validated = (closure.pattern)(value, env, stack)?;

            let value = validated.into_valid().ok_or_else(|| {
                Return::error("Cannot use this value as input to this closure", stack)
            })?;

            let mut inner_env = env::child_of(&closure.captured_env);

            inner_env.set_variable(&closure.parameter.name, value);

            closure.return_value.evaluate(&inner_env.into(), stack)
        })
    });

    // Closure == Text
    env.add_text_relation::<Closure>("closure");
}
