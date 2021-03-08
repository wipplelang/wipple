use crate::*;
use bigdecimal::BigDecimal;

#[derive(Clone)]
pub struct Number {
    pub number: BigDecimal,
    pub location: Option<SourceLocation>,
}

fundamental_primitive!(number for Number);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|number: Number| Text {
        text: number.number.to_string(),
        location: None,
    });
}
