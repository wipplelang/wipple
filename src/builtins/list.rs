use crate::builtins::*;
use crate::fundamentals::*;

#[derive(Clone)]
pub struct List(pub Vec<Value>);

simple_trait! {
    name: list,
    type: List,
    label: "List",
}

pub(crate) fn init(env: &mut Environment) {
    // List ::= Evaluate
    env.add_conformance(Conformance::new(
        TraitID::evaluate,
        TraitID::list.validation(),
        |list, _| {
            let list = list.clone();

            Ok(EvaluateFn::new(move |env| {
                let operators = find_operators(&list, env)?;
                if let Some(parsed) = parse_operators(list.clone(), operators, env)? {
                    return Ok(parsed);
                }

                let mut result = match list.0.first() {
                    Some(value) => value.clone(),
                    None => {
                        // Empty list evaluates to itself
                        return Ok(Value::new(Trait::list(list.clone())));
                    }
                };

                for value in list.0.iter().skip(1) {
                    result = result.call_with(value.clone(), env)?;
                }

                Ok(result)
            }))
        },
    ));

    // List ::= Macro-Expand
    env.add_conformance(Conformance::new(
        TraitID::macro_expand,
        TraitID::list.validation(),
        |list, _| {
            let list = list.clone();

            Ok(MacroExpandFn::new(move |parameter, replacement, env| {
                let new_list = List(
                    list.0
                        .iter()
                        .map(|value| {
                            value.macro_expand(parameter.clone(), replacement.clone(), env)
                        })
                        .collect::<Result<_>>()?,
                );

                Ok(Value::new(Trait::list(new_list)))
            }))
        },
    ));

    // (List and (each Text)) ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::list
            .validation()
            .and(Validation::new(|list: List, env| {
                let mut texts = Vec::with_capacity(list.0.len());

                for value in list.0 {
                    match value.get_trait_if_present(TraitID::text, env)? {
                        Some(text) => texts.push(text.0),
                        None => return Ok(Invalid),
                    }
                }

                Ok(ValidationResult::Valid(texts))
            })),
        |list, _| Ok(Text(format!("({})", list.join(" ")))),
    ));
}
