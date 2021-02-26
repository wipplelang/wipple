use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct EvaluateFn(pub Rc<dyn Fn(&mut Environment, &Stack) -> Result>);

impl EvaluateFn {
    pub fn new(evaluate: impl Fn(&mut Environment, &Stack) -> Result + 'static) -> Self {
        EvaluateFn(Rc::new(evaluate))
    }
}

primitive!(evaluate for EvaluateFn);

impl Value {
    pub fn evaluate(&self, env: &mut Environment, stack: &Stack) -> Result {
        match self.get_primitive_if_present::<EvaluateFn>(env, stack)? {
            Some(evaluate) => evaluate.0(env, stack),
            None => Ok(self.clone()),
        }
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable(
        "evaluate!",
        Value::of(Function::new(|value, env, stack| {
            value.evaluate(env, stack)?.evaluate(env, stack)
        })),
    )
}
