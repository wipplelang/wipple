use crate::*;
use wipple::*;

#[derive(TypeInfo, Debug, Clone)]
pub struct Block {
    pub statements: Vec<List>,
    pub location: Option<SourceLocation>,
}

impl Primitive for Block {}

impl Block {
    pub fn new(statements: Vec<List>) -> Self {
        Block::new_located(statements, None)
    }

    pub fn new_located(statements: Vec<List>, location: Option<SourceLocation>) -> Self {
        Block {
            statements,
            location,
        }
    }
}

stored_closure!(pub struct EvaluateBlockFn(&Block, &Env, &Stack) -> Result<Value>);

impl Default for EvaluateBlockFn {
    fn default() -> Self {
        EvaluateBlockFn::new(|block, env, stack| {
            let env = env.child();
            block.reduce(&env, stack)
        })
    }
}

env_key!(pub evaluate_block: EvaluateBlockFn {
    visibility: EnvKeyVisibility::Private,
});

impl Block {
    pub fn reduce(&self, env: &Env, stack: &Stack) -> Result<Value> {
        let mut stack = stack.clone();
        stack
            .diagnostics_mut()
            .queue_location(self.location.as_ref());

        let mut result = Value::empty();

        for statement in &self.statements {
            let mut stack = stack.clone();
            stack
                .diagnostics_mut()
                .queue_location(statement.location.as_ref());

            // Evaluate each statement as a list
            result = Value::of(statement.clone())
                .evaluate(&env, &stack)?
                .into_owned();
        }

        Ok(result)
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable("Block", Value::of(Trait::of::<Block>()));

    // Block == Evaluate
    env.add_relation_between(stack, |block: Block| {
        EvaluateFn::new(move |env, stack| env.evaluate_block()(&block, env, stack))
    })?;

    // Block == Interpolate
    env.add_relation_between(stack, |block: Block| {
        InterpolateFn::new(move |in_escaped, env, stack| {
            let mut statements = Vec::new();

            for statement in &block.statements {
                // Interpolate within each statement as a list
                let result = Value::of(statement.clone())
                    .interpolate(in_escaped, env, stack)?
                    .into_owned()
                    .into_primitive()
                    .into_cast::<List>();

                statements.push(result);
            }

            Ok(Value::of(Block::new(statements)))
        })
    })?;

    // Block == Text
    env.add_text_relation::<Block>("block", stack)?;

    Ok(())
}
