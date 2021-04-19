use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::*;

#[typeinfo]
#[derive(Debug, Clone)]
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

core_primitive!(pub block for Block);

impl Block {
    pub fn reduce(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let block_env = Environment::child_of(env).into_ref();
        setup_module_block(&block_env);
        self.reduce_inline(&block_env, stack)
    }

    pub fn reduce_inline(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let mut stack = stack.clone();
        stack.evaluation_mut().queue_location(&self.location);

        let mut result = Value::empty();

        for statement in &self.statements {
            let mut stack = stack.clone();
            stack.evaluation_mut().queue_location(&statement.location);

            // Evaluate each statement as a list
            let list = Value::of(statement.clone());
            result = list.evaluate(env, &stack)?;
        }

        Ok(result)
    }
}

pub(crate) fn setup(env: &mut Environment) {
    env.set_variable("Block", Value::of(Trait::of::<Block>()));

    // Block == Text
    env.add_text_conformance::<Block>("block");

    // Block == Replace-In-Template
    env.add_primitive_conformance(|block: Block| {
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

    // Block == Validation
    env.add_conformance(Trait::block(), Trait::validation(), |value, env, stack| {
        let block = value.clone().into_primitive::<Block>();

        let fields = Rc::new(RefCell::new(HashMap::new()));
        let child_env = Environment::child_of(env).into_ref();
        setup_validation_block(fields.clone(), &child_env);

        block.reduce_inline(&child_env, stack)?;

        let fields = fields.take();

        Ok(Value::of(Validation::new(move |value, env, stack| {
            let module = value.get_or::<Module>("Expected module", env, stack)?;

            let mut validated_env = Environment::blank();

            let variables = module.env.borrow_mut().variables().clone();
            for (name, variable) in variables.0 {
                let value = variable.get_value(env, stack)?;

                let validated = match fields.get(&name) {
                    Some(validation) => validation(&value, env, stack)?,
                    None => return Ok(Validated::Invalid),
                };

                match validated {
                    Validated::Valid(value) => validated_env.set_variable(&name, value),
                    Validated::Invalid => return Ok(Validated::Invalid),
                };
            }

            Ok(Validated::Valid(Value::of(Module::new(validated_env))))
        })))
    });
}

fn setup_validation_block(fields: Rc<RefCell<HashMap<String, Validation>>>, env: &EnvironmentRef) {
    *env.borrow_mut().handle_assign() = HandleAssignFn::new(move |left, right, env, stack| {
        let validation =
            right
                .evaluate(env, stack)?
                .get_or::<Validation>("Expected validation", env, stack)?;

        fields.borrow_mut().insert(left.name.clone(), validation);

        Ok(())
    });
}
