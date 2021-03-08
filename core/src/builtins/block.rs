use crate::*;

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<List>,
    pub location: Option<Location>,
}

fundamental_primitive!(block for Block);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance("builtin 'Block ::= Text'", |_: Block| Text {
        text: String::from("<block>"),
        location: None,
    });

    env.add_primitive_conformance("builtin 'Block ::= Evaluate'", |block: Block| {
        EvaluateFn::new(move |env, stack| {
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
        })
    });

    env.add_primitive_conformance("builtin 'Block ::= Macro-Expand'", |block: Block| {
        MacroExpandFn::new(move |parameter, replacement, env, stack| {
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
        })
    });
}
