use wipple::*;
use wipple_stdlib::*;

pub fn setup() -> Result<Stack> {
    let env = Environment::global();
    let mut stack = Stack::new();

    wipple::setup();
    wipple_stdlib::setup(&env, &stack)?;
    wipple_projects::setup();

    *show_mut_in(&mut stack) = ShowFn::new(move |value, env, stack| {
        println!("{}", value.evaluate(env, stack)?.format(env, stack)?);
        Ok(())
    });

    Ok(stack)
}
