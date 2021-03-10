use crate::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub id: TraitID,
    pub validation: Validation,
}

fundamental_primitive!(pub trait_constructor for TraitConstructor);

pub(crate) fn setup(env: &mut Environment) {
    env.add_text_conformance(TraitID::trait_constructor(), "trait");
}
