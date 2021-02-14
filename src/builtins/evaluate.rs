use crate::builtins::*;
use crate::fundamentals::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct EvaluateFn(pub Rc<dyn Fn(&mut Environment) -> Result>);

impl EvaluateFn {
    pub fn new(evaluate: impl Fn(&mut Environment) -> Result + 'static) -> EvaluateFn {
        EvaluateFn(Rc::new(evaluate))
    }
}

simple_trait! {
    name: evaluate,
    type: EvaluateFn,
    label: "Evaluate",
}

impl Value {
    pub fn evaluate(&self, env: &mut Environment) -> Result {
        match self.get_trait_if_present(TraitID::evaluate, env)? {
            Some(evaluate) => evaluate.0(env),
            None => Ok(self.clone()),
        }
    }
}

pub(crate) fn init(env: &mut Environment) {
    env.variables.insert(
        String::from("eval!"),
        Value::new(Trait::function(Function::new(|input, env| {
            input.evaluate(env)?.evaluate(env)
        }))),
    );
}
