use crate::*;

#[derive(Debug, Clone)]
pub struct Module {
    /// The captured environment will have its parent discarded.
    pub env: EnvironmentRef,
}

impl Module {
    pub fn new(mut env: Environment) -> Self {
        env.parent = None;

        Module {
            env: env.into_ref(),
        }
    }
}

core_primitive!(pub module for Module);

impl Block {
    pub fn reduce_into_module(&self, env: &EnvironmentRef, stack: Stack) -> Result {
        // Modules capture their environment
        let module_env = Environment::child_of(env).into_ref();

        let stack = match &self.location {
            Some(location) => stack.update_evaluation(|e| e.queue_location(&location)),
            None => stack,
        };

        setup_module_block(&module_env);

        for statement in &self.statements {
            let stack = match &statement.location {
                Some(location) => stack
                    .clone()
                    .update_evaluation(|e| e.queue_location(&location)),
                None => stack.clone(),
            };

            // Evaluate each statement as a list
            let list = Value::of(statement.clone());
            list.evaluate(&module_env, stack)?;
        }

        let module_env = module_env.borrow().clone();

        Ok(Value::of(Module::new(module_env)))
    }
}

pub fn setup_module_block(env: &EnvironmentRef) {
    *env.borrow_mut().handle_assign() = HandleAssignFn::new(|left, right, env, stack| {
        let value = right.evaluate(env, stack)?;

        env.borrow_mut().set_variable(&left.name, value);

        Ok(())
    })
}

pub(crate) fn setup(env: &mut Environment) {
    // Block == Evaluate
    env.add_primitive_conformance(|block: Block| {
        // Blocks are evaluated as modules by default
        EvaluateFn::new(move |env, stack| block.reduce_into_module(env, stack))
    });

    env.set_variable("Module", Value::of(Trait::of::<Module>()));

    // Module == Text
    env.add_text_conformance::<Module>("module");

    // Module == Function
    env.add_primitive_conformance(|module: Module| {
        Function::new(move |value, env, stack| {
            let name = value.get_primitive_or::<Name>("Expected a name", env, stack.clone())?;
            name.resolve(&module.env, stack)
        })
    });
}
