use crate::builtins::*;
use crate::fundamentals::*;
use num_rational::BigRational;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Number(pub BigRational);

simple_trait! {
    name: number,
    type: Number,
    label: "Number",
}

pub(crate) fn init(env: &mut Environment) {
    // Number ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::number.validation(),
        |number, _| Ok(Text(number.0.to_string())),
    ))
}
