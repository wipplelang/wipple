use crate::builtins::*;
use crate::fundamentals::*;

#[derive(Clone)]
pub struct List {
    pub items: Vec<ListItem>,
    pub location: Option<SourceLocation>,
}

#[derive(Clone)]
pub struct ListItem {
    pub value: Value,
    pub location: Option<SourceLocation>,
}

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
        |list, _, _| {
            let list = list.clone();

            Ok(EvaluateFn::new(move |env, stack| {
                let mut stack = stack.clone();
                if let Some(location) = &list.location {
                    stack.queue_location(location);
                }

                let operators = find_operators(&list, env, &stack)?;
                if let Some(parsed) = parse_operators(list.clone(), operators, env, &stack)? {
                    return Ok(parsed);
                }

                let (mut result, mut callee_location) = match list.items.first() {
                    Some(item) => {
                        let mut stack = stack.clone();
                        if let Some(location) = &item.location {
                            stack.queue_location(location);
                        }

                        let result = item.value.evaluate(env, &stack)?;

                        (result, &item.location)
                    }
                    None => {
                        // Empty list evaluates to itself
                        return Ok(Value::new(Trait::list(list.clone())));
                    }
                };

                for item in list.items.iter().skip(1) {
                    let mut stack = stack.clone();
                    if let Some(location) = callee_location {
                        stack.queue_location(location);
                    }

                    result = result.call_with(item.value.clone(), env, &stack)?;
                    callee_location = &item.location;
                }

                Ok(result)
            }))
        },
    ));

    // List ::= Macro-Expand
    env.add_conformance(Conformance::new(
        TraitID::macro_expand,
        TraitID::list.validation(),
        |list, _, _| {
            let list = list.clone();

            Ok(MacroExpandFn::new(
                move |parameter, replacement, env, stack| {
                    let new_list = List {
                        items: list
                            .items
                            .iter()
                            .map(|item| {
                                let mut stack = stack.clone();
                                if let Some(location) = &item.location {
                                    stack.queue_location(location);
                                }

                                let result = item.value.macro_expand(
                                    parameter.clone(),
                                    replacement.clone(),
                                    env,
                                    &stack,
                                )?;

                                Ok(ListItem {
                                    value: result,
                                    location: item.location.clone(),
                                })
                            })
                            .collect::<Result<_>>()?,
                        location: None,
                    };

                    Ok(Value::new(Trait::list(new_list)))
                },
            ))
        },
    ));

    // (List and (each Text)) ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::list
            .validation()
            .and(Validation::new(|list: List, env, stack| {
                let mut texts = Vec::with_capacity(list.items.len());

                for item in list.items {
                    let mut stack = stack.clone();
                    if let Some(location) = &item.location {
                        stack.queue_location(location);
                    }

                    match item
                        .value
                        .get_trait_if_present(TraitID::text, env, &stack)?
                    {
                        Some(text) => texts.push(text.0),
                        None => return Ok(Invalid),
                    }
                }

                Ok(ValidationResult::Valid(texts))
            })),
        |list, _, _| Ok(Text(format!("({})", list.join(" ")))),
    ));
}
