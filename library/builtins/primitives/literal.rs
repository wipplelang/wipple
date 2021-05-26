use crate::*;

use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Literal {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

impl Primitive for Literal {}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal::new_located(value, None)
    }

    pub fn new_located(value: Value, location: Option<SourceLocation>) -> Self {
        Literal { value, location }
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Literal", Value::of(Trait::of::<Literal>()));

    // Literal == Evaluate
    env.add_relation_between(stack, |literal: Literal| {
        EvaluateFn::new(move |_, _| Ok(literal.value.clone()))
    })?;

    // Literal == Text
    env.add_relation_between_with(stack, |literal: Literal, env, stack| {
        let text = literal.value.format(env, stack)?;
        Ok(Text::new(format!("'{}", text)))
    })?;

    env.set_variable("literal", Value::of(Function::new(|value, _, _| Ok(value))));

    Ok(())
}
