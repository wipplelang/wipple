use crate::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub r#trait: Trait,
    pub validation: Validation,
}

impl TraitConstructor {
    pub fn new(r#trait: Trait, validation: Validation) -> Self {
        TraitConstructor {
            r#trait,
            validation,
        }
    }

    pub fn of<T: Primitive>() -> Self {
        TraitConstructor::new(Trait::of::<T>(), Validation::of::<T>())
    }
}

core_primitive!(pub trait_constructor for TraitConstructor);

pub(crate) fn setup(env: &mut Environment) {
    // Trait ::= Text
    env.add_text_conformance(Trait::trait_constructor(), "trait");
}
