use crate::*;

#[derive(Clone)]
pub struct Module {
    pub env: EnvironmentRef,
}

impl Module {
    pub fn new(env: EnvironmentRef) -> Self {
        Module { env }
    }
}

fundamental_primitive!(module for Module);

#[derive(Clone)]
pub struct ModuleBlock {
    pub statements: Vec<List>,
    pub location: Option<SourceLocation>,
}

fundamental_primitive!(module_block for ModuleBlock);

pub(crate) fn setup(env: &mut Environment) {
    env.add_primitive_conformance(|_: Module| Text {
        text: String::from("<module>"),
        location: None,
    });

    env.add_primitive_conformance(|module: Module| {
        Function::new(move |value, env, stack| {
            let name = value.get_primitive_or::<Name>("Expected a name", env, stack)?;

            name.resolve_in(&module.env, env, stack)
        })
    });

    env.add_primitive_conformance(|_: ModuleBlock| Text {
        text: String::from("<module block>"),
        location: None,
    });

    env.add_primitive_conformance(|module_block: ModuleBlock| {
        EvaluateFn::new(move |env, stack| {
            let mut stack = stack.clone();
            if let Some(location) = &module_block.location {
                stack.queue_location(location);
            }

            // Modules capture their environment
            let captured_env = Environment::child_of(env).into_ref();

            for statement in &module_block.statements {
                let mut stack = stack.clone();
                if let Some(location) = &statement.location {
                    stack.queue_location(location);
                }

                // Evaluate each statement as a list
                let list = Value::of(statement.clone());
                list.evaluate(&captured_env, &stack)?;
            }

            Ok(Value::of(Module::new(captured_env)))
        })
    });

    env.add_primitive_conformance(|module_block: ModuleBlock| {
        MacroExpandFn::new(move |parameter, replacement, env, stack| {
            // Module blocks expand the same way as blocks

            let block = Value::of(Block {
                statements: module_block.statements.clone(),
                location: module_block.location.clone(),
            });

            let expanded_block = block
                .macro_expand(parameter, replacement, env, stack)?
                .get_primitive::<Block>(env, stack)?;

            Ok(Value::of(ModuleBlock {
                statements: expanded_block.statements,
                location: expanded_block.location,
            }))
        })
    });
}
