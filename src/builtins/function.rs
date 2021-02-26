use crate::*;
use std::rc::Rc;

#[derive(Clone)]
pub struct Function(pub Rc<dyn Fn(&Value, &mut Environment, &Stack) -> Result>);

impl Function {
    pub fn new(function: impl Fn(&Value, &mut Environment, &Stack) -> Result + 'static) -> Self {
        Function(Rc::new(function))
    }
}

primitive!(function for Function);

impl Value {
    pub fn call(&self, parameter: &Value, env: &mut Environment, stack: &Stack) -> Result {
        let stack = stack.add(|| format!("Calling '{}'", self.format(env, stack)));

        let function = self.get_primitive_or::<Function>(
            "Cannot call this value because it does not have the Function trait",
            env,
            &stack,
        )?;

        function.0(parameter, env, &stack)
    }
}

pub(crate) fn setup(env: &mut Environment) {
    // Function ::= Text
    env.add_conformance_for_primitive(TraitID::text(), |_: Function, _, _| {
        Ok(Some(Value::of(Text {
            text: String::from("<function>"),
            location: None,
        })))
    });
}
