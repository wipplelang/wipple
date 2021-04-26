use crate::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Closure {
    pub captured_env: EnvironmentRef,
    pub validation: Validation,
    pub parameter: Name,
    pub return_value: Value,
}

core_primitive!(pub closure for Closure);

pub(crate) fn setup(env: &mut Environment) {
    // Closure == Function
    env.add_primitive_conformance(|closure: Closure| {
        Function::new(move |value, env, stack| {
            let value = value.evaluate(env, stack)?;

            let validated = (closure.validation)(&value, env, stack)?;

            let value = match validated {
                Validated::Valid(value) => value,
                Validated::Invalid => {
                    return Err(Return::error(
                        "Cannot use this value as input to this closure",
                        stack,
                    ))
                }
            };

            let inner_env = Environment::child_of(&closure.captured_env).into_ref();

            inner_env
                .borrow_mut()
                .set_variable(&closure.parameter.name, value);

            closure.return_value.evaluate(&inner_env, stack)
        })
    });

    // Closure == Text
    env.add_text_conformance::<Closure>("closure");
}
