use crate::*;

#[derive(Clone)]
pub struct Closure {
    pub captured_env: EnvironmentRef,
    pub define_parameter: AssignFn,
    pub return_value: Value,
}

fundamental_primitive!(pub closure for Closure);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|closure: Closure| {
        Function::new(move |value, _, stack| {
            closure.define_parameter.0(value, &closure.captured_env, stack)?;
            closure.return_value.evaluate(&closure.captured_env, stack)
        })
    });

    env.add_text_conformance(TraitID::closure(), "closure");
}
