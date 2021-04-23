use crate::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Text {
    pub text: String,
    pub location: Option<SourceLocation>,
}

impl Text {
    pub fn new(text: &str) -> Self {
        Text::new_located(text, None)
    }

    pub fn new_located(text: &str, location: Option<SourceLocation>) -> Self {
        Text {
            text: String::from(text),
            location,
        }
    }
}

core_primitive!(pub text for Text);

impl Value {
    pub fn try_format(&self, env: &EnvironmentRef, stack: &Stack) -> String {
        let mut stack = stack.clone();
        stack.evaluation_mut().disable_recording();

        match self.get_if_present::<Text>(env, &stack) {
            Ok(Some(text)) => text.text,
            Ok(None) => String::from("<value>"),
            Err(_) => String::from("<error retrieving text>"),
        }
    }

    pub fn format(&self, env: &EnvironmentRef, stack: &Stack) -> Result<String> {
        let mut stack = stack.clone();
        stack.evaluation_mut().disable_recording();

        Ok(self
            .get_if_present::<Text>(env, &stack)?
            .map(|t| t.text)
            .unwrap_or_else(|| String::from("<value>")))
    }
}

pub(crate) fn setup(env: &mut Environment) {
    // Text : trait
    env.set_variable("Text", Value::of(Trait::of::<Text>()));
}
