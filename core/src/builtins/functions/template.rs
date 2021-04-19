use crate::*;

fn_wrapper_struct! {
    #[typeinfo]
    pub type ReplaceInTemplateFn(&str, &Value, &EnvironmentRef, &Stack) -> Result;
}

core_primitive!(pub replace_in_template for ReplaceInTemplateFn);

impl Value {
    pub fn replace_in_template(
        &self,
        parameter: &str,
        replacement: &Value,
        env: &EnvironmentRef,
        stack: &Stack,
    ) -> Result {
        match self.get_if_present::<ReplaceInTemplateFn>(env, stack)? {
            Some(replace_in_template) => replace_in_template(parameter, replacement, env, stack),
            None => Ok(self.clone()),
        }
    }
}

#[typeinfo]
#[derive(Debug, Clone)]
pub struct Template {
    pub parameter: String,
    pub replace_in: Value,
}

core_primitive!(pub template for Template);

pub(crate) fn setup(env: &mut Environment) {
    // Template == Function
    env.add_primitive_conformance(|template: Template| {
        Function::new(move |replacement, env, stack| {
            template
                .replace_in
                .replace_in_template(&template.parameter, &replacement, env, stack)?
                .evaluate(env, stack)
        })
    });

    // Template == Text
    env.add_text_conformance::<Template>("template");
}
