use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Template {
    pub captured_env: Env,
    pub parameters: Vec<(Pattern, String)>,
    pub return_value: Value,
    pub(crate) partially_applied_inputs: Vec<Value>,
}

impl Primitive for Template {}

impl Template {
    fn expand(&self, env: &Env, stack: &Stack) -> Result<Value> {
        assert!(self.parameters.len() == self.partially_applied_inputs.len());

        let inner_env = self.captured_env.child();

        for ((pattern, name), value) in self.parameters.iter().zip(&self.partially_applied_inputs) {
            let value = pattern(value, env, stack)?
                .ok_or_else(|| error("Cannot use this value as input to this template", stack))?
                .into_owned();

            inner_env.set_variable(stack, name, value)?;
        }

        self.return_value
            .interpolate(true, &inner_env, stack)
            .map(Cow::into_owned)
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    // Template == Function
    env.add_relation_between(stack, |template: Template| {
        Function::new(move |value, env, stack| {
            let mut template = template.clone();
            template.partially_applied_inputs.push(value);

            if template.parameters.len() == template.partially_applied_inputs.len() {
                template
                    .expand(env, stack)?
                    .evaluate(env, stack)
                    .map(Cow::into_owned)
            } else {
                Ok(Value::of(template))
            }
        })
    })?;

    // Template == Text
    env.add_text_relation::<Template>("template", stack)?;

    // TODO: Use 'apply' instead
    env.set_variable(
        stack,
        "expand",
        Value::of(Function::new(move |value, env, stack| {
            let items = value
                .evaluate(env, stack)?
                .get_or::<List>("Expected list", env, stack)?
                .items
                .clone();

            Ok(Value::of(Function::new(move |template, env, stack| {
                let mut template = template
                    .evaluate(env, stack)?
                    .get_or::<Template>("Expected template", env, stack)?
                    .into_owned();

                let expected_count =
                    template.parameters.len() - template.partially_applied_inputs.len();

                if items.len() != expected_count {
                    return Err(error(
                        &format!(
                            "Expected {} parameters, found {}",
                            expected_count,
                            items.len(),
                        ),
                        stack,
                    ));
                }

                template.partially_applied_inputs.append(&mut items.clone());

                template.expand(env, stack)
            })))
        })),
    )?;

    Ok(())
}
