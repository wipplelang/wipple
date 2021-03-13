use crate::*;

#[derive(Clone)]
pub struct Block {
    pub statements: Vec<List>,
    pub location: Option<SourceLocation>,
}

impl Block {
    pub fn new(statements: &[List]) -> Self {
        Block::new_located(statements, None)
    }

    pub fn new_located(statements: &[List], location: Option<SourceLocation>) -> Self {
        Block {
            statements: statements.to_vec(),
            location,
        }
    }
}

fundamental_primitive!(pub block for Block);

pub(crate) fn setup(env: &mut Environment) {
    // Block ::= Text
    env.add_text_conformance(TraitID::block(), "block");

    // Block ::= Evaluate
    env.add_primitive_conformance(|block: Block| {
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

    // Block ::= Macro-Expand
    env.add_primitive_conformance(|block: Block| {
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

            Ok(Value::of(Block::new(&statements)))
        })
    });
}
