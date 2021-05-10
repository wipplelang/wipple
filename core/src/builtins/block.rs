use crate::*;

#[derive(TypeInfo, Debug, Clone)]
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

fn_wrapper! {
    #[derive(TypeInfo)]
    pub struct EvaluateBlockFn(Block, &Environment, &Stack) -> Result;
}

impl Default for EvaluateBlockFn {
    fn default() -> Self {
        EvaluateBlockFn::new(|block, env, stack| block.reduce(env, stack))
    }
}

core_env_key!(pub evaluate_block for EvaluateBlockFn {
    visibility: EnvironmentVisibility::Private,
});

impl Block {
    pub fn reduce(&self, env: &Environment, stack: &Stack) -> Result {
        let mut stack = stack.clone();
        stack.evaluation_mut().queue_location(&self.location);

        let mut result = Value::empty();

        for statement in &self.statements {
            let mut stack = stack.clone();
            stack.evaluation_mut().queue_location(&statement.location);

            // Evaluate each statement as a list

            let list = Value::of(statement.clone());

            match list.evaluate(env, &stack) {
                Ok(value) => result = value,
                Err(r#return) => {
                    return match r#return {
                        Return::Return(value, _) => Ok(value),
                        _ => Err(r#return),
                    }
                }
            };
        }

        Ok(result)
    }
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Block", Value::of(Trait::of::<Block>()));

    // Block == Evaluate
    env.add_primitive_relation(|block: Block| {
        EvaluateFn::new(move |env, stack| {
            let evaluate_block = env.borrow_mut().evaluate_block().clone();
            evaluate_block(block.clone(), env, stack)
        })
    });

    // Block == Text
    env.add_text_relation::<Block>("block");

    // Block == Replace-In-Template
    env.add_primitive_relation(|block: Block| {
        ReplaceInTemplateFn::new(move |parameter, replacement, env, stack| {
            let mut stack = stack.clone();
            stack.evaluation_mut().queue_location(&block.location);

            let mut statements = vec![];

            for statement in &block.statements {
                let mut stack = stack.clone();
                stack.evaluation_mut().queue_location(&statement.location);

                // Expand each statement as a list
                let list = Value::of(statement.clone());
                let expanded = list
                    .replace_in_template(parameter, replacement, env, &stack)?
                    .get::<List>(env, &stack)?;

                statements.push(expanded);
            }

            Ok(Value::of(Block::new(&statements)))
        })
    });
}
