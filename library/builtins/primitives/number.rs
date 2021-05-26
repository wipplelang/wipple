use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Number {
    pub number: f64,
    pub location: Option<SourceLocation>,
}

impl Primitive for Number {}

impl Number {
    pub fn new(number: f64) -> Self {
        Number::new_located(number, None)
    }

    pub fn new_located(number: f64, location: Option<SourceLocation>) -> Self {
        Number { number, location }
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Number", Value::of(Trait::of::<Number>()));

    // Number == Text
    env.add_relation_between(stack, |number: Number| Text::new(number.number.to_string()))?;

    Ok(())
}
