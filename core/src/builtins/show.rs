use std::rc::Rc;

use crate::*;

#[derive(Clone)]
pub struct ShowFn(Rc<dyn Fn(&Value, &EnvironmentRef, &Stack) -> Result<()>>);

impl ShowFn {
    pub fn new(show: impl Fn(&Value, &EnvironmentRef, &Stack) -> Result<()> + 'static) -> Self {
        ShowFn(Rc::new(show))
    }
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
    EnvironmentKey::new(
        UseFn::take_new(),
        true,
    )
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
