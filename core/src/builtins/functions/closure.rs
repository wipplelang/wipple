use crate::*;

#[derive(Clone)]
pub struct Closure {
    pub captured_env: EnvironmentRef,
    pub define_parameter: AssignFn,
    pub return_value: Value,
}

fundamental_primitive!(pub closure for Closure);

pub(crate) fn setup(env: &mut Environment) {
    // Closure ::= Function
    env.add_primitive_conformance(|closure: Closure| {
        Function::new(move |value, _, stack| {
            let inner_env = Environment::child_of(&closure.captured_env).into_ref();

            (closure.define_parameter)(value, false, &inner_env, stack.clone())?;
            closure.return_value.evaluate(&inner_env, stack)
        })
    });

    // Closure ::= Text
    env.add_text_conformance(ID::closure(), "closure");
}
