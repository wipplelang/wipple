use crate::fundamentals::*;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Text(pub String);

simple_trait! {
    name: text,
    type: Text,
    label: "Text",
}

impl Value {
    pub fn format(&self, env: &mut Environment, stack: &ProgramStack) -> String {
        let mut stack = stack.clone();
        stack.disable_diagnostics();

        match self.get_trait_if_present(TraitID::text, env, &stack) {
            Ok(text) => text.map_or_else(|| String::from("<value>"), |text| text.0),
            Err(_) => String::from("<error retrieving text>"),
        }
    }
}
