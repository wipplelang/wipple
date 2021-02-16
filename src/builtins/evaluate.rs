use crate::builtins::*;
use crate::fundamentals::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct EvaluateFn(pub Rc<dyn Fn(&mut Environment, &ProgramStack) -> Result>);

impl EvaluateFn {
    pub fn new(
        evaluate: impl Fn(&mut Environment, &ProgramStack) -> Result + 'static,
    ) -> EvaluateFn {
        EvaluateFn(Rc::new(evaluate))
    }
}

simple_trait! {
    name: evaluate,
    type: EvaluateFn,
    label: "Evaluate",
}

impl Value {
    pub fn evaluate(&self, env: &mut Environment, stack: &ProgramStack) -> Result {
        let stack = stack.add(|| format!("Evaluating '{}'", self.format(env, stack)));

        match self.get_trait_if_present(TraitID::evaluate, env, &stack)? {
            Some(evaluate) => evaluate.0(env, &stack),
            None => Ok(self.clone()),
        }
    }
}

pub(crate) fn init(env: &mut Environment) {
    env.variables.insert(
        String::from("eval!"),
        Value::new(Trait::function(Function::new(|input, env, stack| {
            input.evaluate(env, stack)?.evaluate(env, stack)
        }))),
    );
}
