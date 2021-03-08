use crate::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub id: TraitID,
    pub validation: Validation,
}

fundamental_primitive!(trait_constructor for TraitConstructor);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|_: TraitConstructor| Text {
        text: String::from("<trait>"),
        location: None,
    });
}
