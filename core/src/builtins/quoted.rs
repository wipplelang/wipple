use crate::*;

#[derive(TypeInfo, Debug, Clone)]
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

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Quoted", Value::of(Trait::of::<Quoted>()));

    // Quoted == Evaluate
    env.add_primitive_conformance(|quoted: Quoted| {
        EvaluateFn::new(move |_, _| Ok(quoted.value.clone()))
    });

    // Quoted == Replace-In-Template
    env.add_primitive_conformance(|quoted: Quoted| {
        ReplaceInTemplateFn::new(move |parameter, replacement, env, stack| {
            let replaced = quoted
                .value
                .replace_in_template(parameter, replacement, env, stack)?;

            Ok(Value::of(Quoted::new(replaced)))
        })
    });

    // Quoted == Text
    env.add_conformance(
        Validation::for_trait(Trait::of::<Quoted>()),
        Trait::of::<Text>(),
        |value, env, stack| {
            let quoted = value.into_primitive::<Quoted>().unwrap();

            let text = quoted.value.format(env, stack)?;

            Ok(Value::of(Text::new(&format!("'{}", text))))
        },
    );
}
