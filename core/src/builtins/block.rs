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

impl Block {
    pub fn reduce(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        let mut block_env = Environment::child_of(env);
        setup_module_block(&mut block_env);

        let block_env = block_env.into_ref();

        self.reduce_inline(&block_env, stack)
    }

    pub fn reduce_inline(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        let stack = match &self.location {
            Some(location) => stack.update_evaluation(|e| e.queue_location(&location)),
            None => stack,
        };

        let mut result = Value::empty();

        for statement in &self.statements {
            let stack = match &statement.location {
                Some(location) => stack
                    .clone()
                    .update_evaluation(|e| e.queue_location(&location)),
                None => stack.clone(),
            };

            // Evaluate each statement as a list
            let list = Value::of(statement.clone());
            result = list.evaluate(env, stack)?;
        }

        Ok(result)
    }
}

pub(crate) fn setup(env: &mut Environment) {
    // Block ::= Text
    env.add_text_conformance(ID::block(), "block");

    // Block ::= Replace-In-Template
    env.add_primitive_conformance(|block: Block| {
        ReplaceInTemplateFn::new(move |parameter, replacement, env, stack| {
            let stack = match &block.location {
                Some(location) => stack.update_evaluation(|e| e.queue_location(&location)),
                None => stack,
            };

            let mut statements = vec![];

            for statement in &block.statements {
                let stack = match &statement.location {
                    Some(location) => stack
                        .clone()
                        .update_evaluation(|e| e.queue_location(&location)),
                    None => stack.clone(),
                };

                // Expand each statement as a list
                let list = Value::of(statement.clone());
                let expanded = list
                    .replace_in_template(parameter, replacement, env, stack.clone())?
                    .get_primitive::<List>(env, stack)?;

                statements.push(expanded);
            }

            Ok(Value::of(Block::new(&statements)))
        })
    });
}
