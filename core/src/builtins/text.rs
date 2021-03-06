use crate::*;

#[derive(Clone)]
pub struct Text {
    pub text: String,
    pub location: Option<SourceLocation>,
}

fundamental_primitive!(text for Text);

impl Value {
    pub fn format(&self, env: &EnvironmentRef, stack: &Stack) -> String {
        let mut stack = stack.clone();
        stack.disable_recording();

        match self.get_primitive_if_present::<Text>(env, &stack) {
            Ok(Some(text)) => text.text,
            Ok(None) => String::from("<value>"),
            Err(_) => String::from("<error retrieving text>"),
        }
    }
}
