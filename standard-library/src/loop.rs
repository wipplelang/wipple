use crate::*;

pub fn setup(env: &EnvironmentRef) {
    env.borrow_mut().set_variable(
        "loop",
        Value::of(Function::new(|value, env, stack| {
            let block = value.get_or::<Block>("Expected block", env, stack)?;

            loop {
                match block.do_inline(env, stack) {
                    Ok(_) => {}
                    Err(r#return) => {
                        return match r#return {
                            Return::Break(value, _) => Ok(value),
                            _ => Err(r#return),
                        }
                    }
                }
            }
        })),
    );

    env.borrow_mut().set_variable(
        "break",
        Value::of(Function::new(|value, env, stack| {
            let value = value.evaluate(env, stack)?;
            Err(Return::r#break(value, stack))
        })),
    );
}
