use crate::*;

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct ReplaceInTemplateFn(&str, &Value, &Environment, &Stack) -> Result;
}

impl Value {
    pub fn replace_in_template(
        &self,
        parameter: &str,
        replacement: &Value,
        env: &Environment,
        stack: &Stack,
    ) -> Result {
        match self.get_if_present::<ReplaceInTemplateFn>(env, stack)? {
            Some(replace_in_template) => replace_in_template(parameter, replacement, env, stack),
            None => Ok(self.clone()),
        }
    }
}

#[derive(TypeInfo, Debug, Clone)]
pub struct Template {
    pub parameter: String,
    pub replace_in: Value,
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    // Template == Function
    env.add_primitive_relation(|template: Template| {
        Function::new(move |replacement, env, stack| {
            template
                .replace_in
                .replace_in_template(&template.parameter, &replacement, env, stack)?
                .evaluate(env, stack)
        })
    });

    // Template == Text
    env.add_text_relation::<Template>("template");
}
