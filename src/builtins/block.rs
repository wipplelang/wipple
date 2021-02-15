use crate::builtins::*;
use crate::fundamentals::*;

#[derive(Clone)]
pub struct Block(pub Vec<Vec<Value>>);

simple_trait! {
    name: block,
    type: Block,
    label: "Block",
}

pub(crate) fn init(env: &mut Environment) {
    // Block ::= Text
    env.add_conformance(Conformance::new(
        TraitID::text,
        TraitID::block.validation(),
        |_, _, _| Ok(Text(String::from("<block>"))),
    ));

    // Block ::= Evaluate
    env.add_conformance(Conformance::new(
        TraitID::evaluate,
        TraitID::block.validation(),
        |block, _, _| {
            let block = block.clone();

            Ok(EvaluateFn::new(move |env, stack| {
                block
                    .0
                    .iter()
                    .map(|statement| {
                        // Evaluate each statement as a list
                        let list = Value::new(Trait::list(List(statement.clone())));
                        list.evaluate(env, stack)
                    })
                    .last()
                    .unwrap_or_else(|| Ok(Value::empty()))
            }))
        },
    ));

    // Block ::= Macro-Expand
    env.add_conformance(Conformance::new(
        TraitID::macro_expand,
        TraitID::block.validation(),
        |block, _, _| {
            let block = block.clone();

            Ok(MacroExpandFn::new(
                move |parameter, replacement, env, stack| {
                    let statements = block
                        .0
                        .iter()
                        .map(|statement| {
                            // Evaluate each statement as a list
                            let list = Value::new(Trait::list(List(statement.clone())));
                            let expanded = list
                                .macro_expand(parameter.clone(), replacement.clone(), env, stack)?
                                .get_trait(TraitID::list, env, stack)?;

                            Ok(expanded.0)
                        })
                        .collect::<Result<_>>()?;

                    Ok(Value::new(Trait::block(Block(statements))))
                },
            ))
        },
    ));
}
