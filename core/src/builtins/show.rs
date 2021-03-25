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

fundamental_env_key!(pub show for ShowFn {
    // The 'show' implementation must be declared in the global environment
    visibility: EnvironmentVisibility::Private,
});

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable(
        "show",
        Value::of(Function::new(|value, env, stack| {
            let show = Environment::global().borrow_mut().show().clone();
            show.0(value, env, stack)?;

            Ok(Value::empty())
        })),
    );
}
