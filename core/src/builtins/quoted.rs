use crate::*;

#[derive(Clone)]
pub struct Quoted {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

impl Quoted {
    pub fn new(value: Value) -> Self {
        Quoted::new_located(value, None)
    }

    pub fn new_located(value: Value, location: Option<SourceLocation>) -> Self {
        Quoted { value, location }
    }
}

fundamental_primitive!(pub quoted for Quoted);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|quoted: Quoted| {
        EvaluateFn::new(move |_, _| Ok(quoted.value.clone()))
    })
}
