use crate::*;

#[derive(TypeInfo, Debug, Clone, Copy)]
struct Empty;

impl Value {
    pub fn empty() -> Self {
        Value::of(Empty)
    }

    pub fn is_empty(&self) -> bool {
        self.is_trait_directly(&Trait::of::<Empty>())
    }
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Empty", Value::empty());

    // _ : <empty value>
    env.set_constant_variable("_", Value::empty());

    // Allow the use of '_' as a catch-all pattern that returns its input
    env.add_relation(
        Pattern::for_empty_value(),
        Trait::of::<Pattern>(),
        move |_, _, _| Ok(Value::of(Pattern::any())),
    );

    // Empty == Text
    env.add_relation(
        Pattern::for_empty_value(),
        Trait::of::<Text>(),
        |_, _, _| Ok(Value::of(Text::new("<empty>"))),
    );
}
