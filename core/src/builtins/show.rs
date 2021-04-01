use crate::*;

fn_wrapper_struct! {
    pub type ShowFn(&Value, &EnvironmentRef, Stack) -> Result<()>;
}

impl Default for ShowFn {
    fn default() -> Self {
        ShowFn::new(|_, _, stack| {
            Err(ReturnState::Error(Error::new(
                "Cannot use 'show' because this runtime does not handle output",
                stack,
            )))
        })
    }
}

core_stack_key!(pub show for ShowFn);

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable(
        "show",
        Value::of(Function::new(|value, env, stack| {
            stack.clone().get_show()(value, env, stack)?;

            Ok(Value::empty())
        })),
    );
}
