use crate::*;

#[derive(Clone)]
pub struct Quoted {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

fundamental_primitive!(quoted for Quoted);

pub(crate) fn setup(env: &mut Environment) {
    // Quoted ::= Evaluate
    env.add_conformance_for_primitive(TraitID::evaluate(), |quoted: Quoted, _, _| {
        Ok(Some(Value::of(EvaluateFn::new(move |_, _| {
            Ok(quoted.value.clone())
        }))))
    })
}
