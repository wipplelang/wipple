use crate::*;
use wipple::*;

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "Pattern", Value::of(Trait::of::<Pattern>()))?;

    // Pattern == Text
    env.add_text_relation::<Pattern>("pattern", stack)?;

    Ok(())
}
