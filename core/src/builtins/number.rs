use crate::*;
use bigdecimal::BigDecimal;

#[derive(Clone)]
pub struct Number {
    pub number: BigDecimal,
    pub location: Option<SourceLocation>,
}

impl Number {
    pub fn new(number: BigDecimal) -> Self {
        Number::new_located(number, None)
    }

    pub fn new_located(number: BigDecimal, location: Option<SourceLocation>) -> Self {
        Number { number, location }
    }
}

core_primitive!(pub number for Number);

pub(crate) fn setup(env: &mut Environment) {
    // Number == Text
    env.add_primitive_conformance(|number: Number| Text::new(&number.number.to_string()));
}
