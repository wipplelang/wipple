use crate::*;

fn_wrapper_struct! {
    #[typeinfo]
    pub type EvaluateFn(&EnvironmentRef, &Stack) -> Result;
}

core_primitive!(pub evaluate for EvaluateFn);

impl Value {
    pub fn evaluate(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        match self.get_if_present::<EvaluateFn>(env, stack)? {
            Some(evaluate) => evaluate.0(env, stack),
            None => Ok(self.clone()),
        }
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Evaluate", Value::of(Trait::of::<EvaluateFn>()));

    env.set_variable(
        "evaluate!",
        Value::of(Function::new(|value, env, stack| {
            value.evaluate(env, stack)?.evaluate(env, stack)
        })),
    )
}
