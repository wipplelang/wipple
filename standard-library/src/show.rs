use crate::*;

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct ShowFn(Value, &Environment, &Stack) -> Result<()>;
}

impl Default for ShowFn {
    fn default() -> Self {
        ShowFn::new(|_, _, stack| {
            Err(Return::error(
                "Cannot use 'show' because this runtime does not handle output",
                stack,
            ))
        })
    }
}

stack_key!(pub show for ShowFn);

pub fn setup(env: &Environment) {
    env.borrow_mut().set_variable(
        "show",
        Value::of(Function::new(|value, env, stack| {
            show_in(stack)(value, env, stack)?;

            Ok(Value::empty())
        })),
    );
}
