use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Template {
    pub captured_env: Env,
    pub parameter_pattern: Pattern,
    pub parameter_name: String,
    pub return_value: Value,
}

impl Primitive for Template {}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    // Template == Function
    env.add_relation_between(stack, |_template: Template| {
        Function::new(move |_value, _env, _stack| {
            todo!();
        })
    })?;

    // Template == Text
    env.add_text_relation::<Template>("template", stack)?;

    Ok(())
}
