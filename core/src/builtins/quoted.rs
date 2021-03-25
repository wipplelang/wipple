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
    // Quoted ::= Evaluate
    env.add_primitive_conformance(|quoted: Quoted| {
        EvaluateFn::new(move |_, _| Ok(quoted.value.clone()))
    });

    // Quoted ::= Text
    env.add_conformance(ID::text(), |value, env, stack| {
        let quoted = match value.get_primitive_if_present::<Quoted>(env, stack.clone())? {
            Some(quoted) => quoted,
            None => return Ok(None),
        };

        Ok(Some(Value::of(Text::new(&format!(
            "'{}",
            quoted.value.format(env, stack)?
        )))))
    });
}
