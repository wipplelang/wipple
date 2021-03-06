use crate::*;

#[derive(Clone)]
pub struct TraitConstructor {
    pub id: TraitID,
    pub validation: Validation,
}

fundamental_primitive!(trait_constructor for TraitConstructor);

pub(crate) fn setup(env: &mut Environment) {
    // Trait ::= Text
    env.add_conformance_for_primitive(TraitID::text(), |_: TraitConstructor, _, _| {
        Ok(Some(Value::of(Text {
            text: String::from("<trait>"),
            location: None,
        })))
    });
}
