use crate::*;

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<List>,
    pub location: Option<SourceLocation>,
}

primitive!(block for Block);

pub(crate) fn setup(env: &mut Environment) {
    // Block ::= Text
    env.add_conformance_for_primitive(TraitID::text(), |_: Block, _, _| {
        Ok(Some(Value::of(Text {
            text: String::from("<block>"),
            location: None,
        })))
    });

    // Block ::= Text
    env.add_conformance_for_primitive(TraitID::evaluate(), |block: Block, _, _| {
        Ok(Some(Value::of(EvaluateFn::new(move |env, stack| {
            let mut stack = stack.clone();
            if let Some(location) = &block.location {
                stack.queue_location(location);
            }

            let mut result = Value::empty();

            for statement in &block.statements {
                let mut stack = stack.clone();
                if let Some(location) = &statement.location {
                    stack.queue_location(location);
                }

                // Evaluate each statement as a list
                let list = Value::of(statement.clone());
                result = list.evaluate(env, &stack)?;
            }

            Ok(result)
        }))))
    });

    // Block ::= Macro-Expand
    env.add_conformance_for_primitive(TraitID::macro_expand(), |block: Block, _, _| {
        Ok(Some(Value::of(MacroExpandFn::new(
            move |parameter, replacement, env, stack| {
                let mut stack = stack.clone();
                if let Some(location) = &block.location {
                    stack.queue_location(location);
                }

                let mut statements = vec![];

                for statement in &block.statements {
                    let mut stack = stack.clone();
                    if let Some(location) = &statement.location {
                        stack.queue_location(location);
                    }

                    // Expand each statement as a list
                    let list = Value::of(statement.clone());
                    let expanded = list
                        .macro_expand(parameter, replacement, env, &stack)?
                        .get_primitive::<List>(env, &stack)?;

                    statements.push(expanded);
                }

                Ok(Value::of(Block {
                    statements,
                    location: None,
                }))
            },
        ))))
    });
}
