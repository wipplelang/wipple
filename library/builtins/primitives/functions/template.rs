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
    env.add_relation_between(stack, |template: Template| {
        Function::new(move |value, env, stack| {
            let validated = (template.parameter_pattern)(&value, env, stack)?;

            let value = validated
                .ok_or_else(|| error("Cannot use this value as input to this closure", stack))?;

            let inner_env = template.captured_env.child();
            inner_env.set_variable(&template.parameter_name, value.into_owned());

            template
                .return_value
                .evaluate(&inner_env, stack)?
                .evaluate(env, stack)
                .map(Cow::into_owned)
        })
    })?;

    // Template == Text
    env.add_text_relation::<Template>("template", stack)?;

    Ok(())
}
