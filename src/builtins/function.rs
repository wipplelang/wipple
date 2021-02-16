use crate::fundamentals::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Function(pub Rc<dyn Fn(Value, &mut Environment, &ProgramStack) -> Result>);

impl Function {
    pub fn new(
        function: impl Fn(Value, &mut Environment, &ProgramStack) -> Result + 'static,
    ) -> Function {
        Function(Rc::new(function))
    }
}

simple_trait! {
    name: function,
    type: Function,
    label: "Function",
}

impl Value {
    pub fn call_with(
        &self,
        parameter: Value,
        env: &mut Environment,
        stack: &ProgramStack,
    ) -> Result {
        let stack = stack.add(|| format!("Calling '{}'", self.format(env, stack)));

        let function = self.get_trait_or(
            TraitID::function,
            "Cannot call this value because it does not have the Function trait",
            env,
            &stack,
        )?;

        function.0(parameter, env, &stack)
    }
}
