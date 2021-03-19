mod closure;
mod template;

pub use closure::*;
pub use template::*;

use crate::*;

fn_wrapper_struct! {
    pub type Function(&Value, &EnvironmentRef, &Stack) -> Result;
}

fundamental_primitive!(pub function for Function);

impl Value {
    pub fn call(&self, parameter: &Value, env: &EnvironmentRef, stack: &Stack) -> Result {
        let stack = stack.add(|| format!("Calling '{}'", self.try_format(env, stack)));

        let function = self.get_primitive_or::<Function>(
            "Cannot call this value because it does not have the Function trait",
            env,
            &stack,
        )?;

        function.0(parameter, env, &stack)
    }
}

pub(crate) fn setup(env: &mut Environment) {
    closure::setup(env);
    template::setup(env);
}
