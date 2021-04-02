use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

core_primitive!(pub block for Block);

impl Block {
    pub fn reduce(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        let block_env = Environment::child_of(env).into_ref();
        setup_module_block(&block_env);
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
    env.set_variable("Block", Value::of(Trait::of::<Block>()));

    // Block == Text
    env.add_text_conformance(Trait::block(), "block");

    // Block == Replace-In-Template
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

    // Block == Validation
    env.add_conformance(Trait::block(), Trait::validation(), |value, env, stack| {
        let block = value.clone().into_primitive::<Block>();

        let fields = Rc::new(RefCell::new(HashMap::new()));
        let child_env = Environment::child_of(env).into_ref();
        setup_validation_block(fields.clone(), &child_env);

        block.reduce_inline(&child_env, stack)?;

        let fields = fields.take();

        Ok(Value::of(Validation::new(move |value, env, stack| {
            let module = value.get_primitive_or::<Module>("Expected module", env, stack.clone())?;

            let mut validated_env = Environment::blank();

            let variables = module.env.borrow_mut().variables().clone();
            for (name, variable) in variables {
                let value = variable.get_value(env, stack.clone())?;

                let validated = match fields.get(&name) {
                    Some(validation) => validation(&value, env, stack.clone())?,
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
    *env.borrow_mut().handle_assign() =
        HandleAssignFn::new(move |left, right, computed, env, stack| {
            let name =
                left.get_primitive_or::<Name>("Expected name for match", env, stack.clone())?;

            if computed {
                return Err(ReturnState::Error(Error::new(
                    "Lazy evaluation is not supported for validations",
                    stack,
                )));
            }

            let validation =
                right.get_primitive_or::<Validation>("Expected validation", env, stack)?;

            fields.borrow_mut().insert(name.name, validation);

            Ok(())
        })
}
