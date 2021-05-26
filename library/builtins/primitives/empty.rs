use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone, Copy)]
struct Empty;

impl Primitive for Empty {}

#[ext(pub, name = ValueEmptyExt)]
impl Value {
    fn empty() -> Self {
        Value::of(Empty)
    }

    fn is_empty(&self) -> bool {
        self.r#trait().as_ref() == &Trait::of::<Empty>()
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Empty", Value::of(Trait::of::<Empty>()));

    // _ : <empty value>
    env.set_constant_variable("_", Value::empty());

    // Allow the use of '_' as a catch-all pattern that returns its input
    env.add_relation_between(stack, |_: Empty| Pattern::any())?;

    // Empty == Text
    env.add_relation_between(stack, |_: Empty| Text::new("<empty>".to_string()))?;

    Ok(())
}
