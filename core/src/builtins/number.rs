use crate::*;
use bigdecimal::BigDecimal;

#[derive(Clone)]
pub struct Number {
    pub number: BigDecimal,
    pub location: Option<Location>,
}

fundamental_primitive!(number for Number);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance("builtin 'Number ::= Text'", |number: Number| Text {
        text: number.number.to_string(),
        location: None,
    });
}
