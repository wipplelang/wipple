use crate::*;

#[derive(Clone)]
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

fundamental_primitive!(pub module for Module);

impl Block {
    pub fn evaluate_as_module(&self, env: &EnvironmentRef, stack: &Stack) -> Result {
        let mut stack = stack.clone();
        if let Some(location) = &self.location {
            stack.queue_location(location);
        }

        // Modules capture their environment
        let mut module_env = Environment::child_of(env);
        setup_module_block(&mut module_env);

        let captured_env = module_env.into_ref();

        for statement in &self.statements {
            let mut stack = stack.clone();
            if let Some(location) = &statement.location {
                stack.queue_location(location);
            }

            // Evaluate each statement as a list
            let list = Value::of(statement.clone());
            list.evaluate(&captured_env, &stack)?;
        }

        let captured_env = captured_env.borrow().clone();

        Ok(Value::of(Module::new(captured_env)))
    }
}

pub(crate) fn setup_module_block(env: &mut Environment) {
    *env.handle_assign() = HandleAssignFn::new(|left, right, env, stack| {
        let stack = stack.add(|| {
            format!(
                "Assigning '{}' to '{}'",
                right.try_format(env, stack),
                left.try_format(env, stack)
            )
        });

        let assign = left.get_primitive_or::<AssignFn>(
            "Cannot assign to this value because it does not have the Assign trait",
            env,
            &stack,
        )?;

        assign(&right, env, &stack)
    })
}

pub(crate) fn setup(env: &mut Environment) {
    // Block ::= Evaluate
    env.add_primitive_conformance(|block: Block| {
        // Blocks are evaluated as modules by default
        EvaluateFn::new(move |env, stack| block.evaluate_as_module(env, stack))
    });

    // Module ::= Text
    env.add_text_conformance(TraitID::module(), "module");

    // Module ::= Function
    env.add_primitive_conformance(|module: Module| {
        Function::new(move |value, env, stack| {
            let name = value.get_primitive_or::<Name>("Expected a name", env, stack)?;

            name.resolve_in(&module.env, env, stack)
        })
    });
}
