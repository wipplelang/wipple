use crate::*;

fn_wrapper_struct! {
    pub type EvaluateFn(&EnvironmentRef, Stack) -> Result;
}

fundamental_primitive!(pub evaluate for EvaluateFn);

impl Value {
    pub fn evaluate(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        match self.get_primitive_if_present::<EvaluateFn>(env, stack.clone())? {
            Some(evaluate) => evaluate.0(env, stack),
            None => Ok(self.clone()),
        }
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable(
        "evaluate!",
        Value::of(Function::new(|value, env, stack| {
            value.evaluate(env, stack.clone())?.evaluate(env, stack)
        })),
    )
}
