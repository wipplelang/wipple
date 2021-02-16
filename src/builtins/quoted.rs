use crate::builtins::*;
use crate::fundamentals::*;

#[derive(Clone)]
pub struct Quoted {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

simple_trait! {
    name: quoted,
    type: Quoted,
    label: "Quoted",
}

pub(crate) fn init(env: &mut Environment) {
    // Quoted ::= Evaluate
    env.add_conformance(Conformance::new(
        TraitID::evaluate,
        TraitID::quoted.validation(),
        move |quoted, _, _| {
            let quoted = quoted.clone();

            Ok(EvaluateFn::new(move |_, _| Ok(quoted.value.clone())))
        },
    ));

    // (Quoted and Macro-Parameter) ::= Macro-Parameter
    env.add_conformance(Conformance::new(
        TraitID::macro_parameter,
        TraitID::quoted
            .validation()
            .join(Validation::new(|quoted: Quoted, env, stack| {
                TraitID::macro_parameter
                    .validation()
                    .validate(quoted.value, env, stack)
            })),
        |(quoted, define_parameter), _, _| {
            let quoted = quoted.clone();
            let define_parameter = define_parameter.clone();

            Ok(DefineMacroParameterFn::new(move |input, env, stack| {
                let mut stack = stack.clone();
                if let Some(location) = &quoted.location {
                    stack.queue_location(location);
                }

                define_parameter.0(
                    Value::new(Trait::quoted(Quoted {
                        value: input,
                        location: None,
                    })),
                    env,
                    &stack,
                )
            }))
        },
    ));

    // Quoted ::= Macro-Expand
    env.add_conformance(Conformance::new(
        TraitID::macro_expand,
        TraitID::quoted.validation(),
        |quoted, _, _| {
            let quoted = quoted.clone();

            Ok(MacroExpandFn::new(
                move |parameter, replacement, env, stack| {
                    let mut stack = stack.clone();
                    if let Some(location) = &quoted.location {
                        stack.queue_location(location);
                    }

                    let value = quoted
                        .value
                        .macro_expand(parameter, replacement, env, &stack)?;

                    Ok(Value::new(Trait::quoted(Quoted {
                        value,
                        location: None,
                    })))
                },
            ))
        },
    ));

    // (Quoted and Text) ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::quoted
            .validation()
            .and(Validation::new(|quoted: Quoted, env, stack| {
                TraitID::text
                    .validation()
                    .validate(quoted.value, env, stack)
            })),
        |text, _, _| Ok(Text(format!("'{}", text.0))),
    ));
}
