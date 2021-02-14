use crate::builtins::*;
use crate::fundamentals::*;

simple_trait! {
    name: quoted,
    type: Value,
    label: "Quoted",
}

pub(crate) fn init(env: &mut Environment) {
    // Quoted ::= Evaluate
    env.add_conformance(Conformance::new(
        TraitID::evaluate,
        TraitID::quoted.validation(),
        move |quoted, _| {
            let quoted = quoted.clone();

            Ok(EvaluateFn::new(move |_| Ok(quoted.clone())))
        },
    ));

    // (Quoted and Macro-Parameter) ::= Macro-Parameter
    env.add_conformance(Conformance::new(
        TraitID::macro_parameter,
        TraitID::quoted
            .validation()
            .and(TraitID::macro_parameter.validation()),
        |define_parameter, _| {
            let define_parameter = define_parameter.clone();

            Ok(DefineMacroParameterFn::new(move |input, env| {
                define_parameter.0(Value::new(Trait::quoted(input)), env)
            }))
        },
    ));

    // Quoted ::= Macro-Expand
    env.add_conformance(Conformance::new(
        TraitID::macro_expand,
        TraitID::quoted.validation(),
        |quoted_value, _| {
            let quoted_value = quoted_value.clone();

            Ok(MacroExpandFn::new(move |parameter, replacement, env| {
                let value = quoted_value.macro_expand(parameter, replacement, env)?;

                Ok(Value::new(Trait::quoted(value)))
            }))
        },
    ));

    // (Quoted and Text) ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::quoted.validation().and(TraitID::text.validation()),
        |text, _| Ok(Text(format!("'{}", text.0))),
    ));
}
