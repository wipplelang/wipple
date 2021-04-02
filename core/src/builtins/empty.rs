use crate::*;

#[derive(Clone, Copy)]
struct Empty;

core_primitive!(pub empty for Empty);

impl Value {
    pub fn empty() -> Self {
        Value::of(Empty)
    }

    pub fn is_empty(&self) -> bool {
        self.r#trait == Trait::of::<Empty>()
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Empty", Value::of(Trait::of::<Empty>()));

    // _ : <empty value>
    env.set_variable("_", Value::empty());

    // Allow the use of '_' as a catch-all validation that returns its input
    env.add_conformance(Trait::empty(), Trait::validation(), move |_, _, _| {
        Ok(Value::of(Validation::any()))
    });

    // Empty == Text
    env.add_text_conformance(Trait::empty(), "empty")
}
