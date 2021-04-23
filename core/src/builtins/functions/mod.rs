mod closure;
mod template;

pub use closure::*;
pub use template::*;

use crate::*;

fn_wrapper_struct! {
    #[derive(TypeInfo)]
    pub type Function(Value, &EnvironmentRef, &Stack) -> Result;
}

core_primitive!(pub function for Function);

impl Value {
    pub fn call(&self, parameter: Value, env: &EnvironmentRef, stack_: &Stack) -> Result {
        let mut stack = stack_.clone();
        stack
            .evaluation_mut()
            .set(|| format!("Calling '{}'", self.try_format(env, &stack_)));

        let function = self.get_or::<Function>(
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

    env.set_variable("Function", Value::of(Trait::of::<Function>()));
}
