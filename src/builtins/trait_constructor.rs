use crate::builtins::*;
use crate::fundamentals::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub id: AnyTraitID,
    pub validation: AnyValidation,
}

impl TraitConstructor {
    pub fn new<T: Clone>(id: TraitID<T>, validation: Validation<Value, T>) -> TraitConstructor {
        TraitConstructor {
            id: AnyTraitID::from(id),
            validation: AnyValidation::from(validation),
        }
    }
}

simple_trait! {
    name: trait_constructor,
    type: TraitConstructor,
    label: "Trait",
}

pub(crate) fn init(env: &mut Environment) {
    // Trait ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::trait_constructor.validation(),
        |_, _| Ok(Text(String::from("<trait>"))),
    ))
}
