use crate::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub id: TraitID,
    pub validation: Validation,
}

impl TraitConstructor {
    pub fn new(id: TraitID, validation: Validation) -> Self {
        TraitConstructor { id, validation }
    }

    pub fn of<T: Primitive>() -> Self {
        TraitConstructor::new(TraitID::of::<T>(), Validation::of::<T>())
    }
}

fundamental_primitive!(pub trait_constructor for TraitConstructor);

pub(crate) fn setup(env: &mut Environment) {
    // Trait ::= Text
    env.add_text_conformance(TraitID::trait_constructor(), "trait");
}
