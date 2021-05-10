use crate::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Literal {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal::new_located(value, None)
    }

    pub fn new_located(value: Value, location: Option<SourceLocation>) -> Self {
        Literal { value, location }
    }
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Literal", Value::of(Trait::of::<Literal>()));

    // Literal == Evaluate
    env.add_primitive_relation(|literal: Literal| {
        EvaluateFn::new(move |_, _| Ok(literal.value.clone()))
    });

    // Literal == Text
    env.add_relation(
        Pattern::for_trait(Trait::of::<Literal>()),
        Trait::of::<Text>(),
        |value, env, stack| {
            let literal = value.into_primitive::<Literal>().unwrap();

            let text = literal.value.format(env, stack)?;

            Ok(Value::of(Text::new(&format!("'{}", text))))
        },
    );

    env.set_variable("literal", Value::of(Function::new(|value, _, _| Ok(value))));
}
