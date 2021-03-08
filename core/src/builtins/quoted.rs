use crate::*;

#[derive(Clone)]
pub struct Quoted {
    pub value: Value,
    pub location: Option<Location>,
}

fundamental_primitive!(quoted for Quoted);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance("builtin 'Quoted ::= Evaluate'", |quoted: Quoted| {
        EvaluateFn::new(move |_, _| Ok(quoted.value.clone()))
    })
}
