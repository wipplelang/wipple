use crate::*;

#[derive(Clone)]
pub struct ModuleBlock {
    pub statements: Vec<List>,
    pub location: Option<SourceLocation>,
}

impl ModuleBlock {
    pub fn new(statements: &[List]) -> Self {
        ModuleBlock::new_located(statements, None)
    }

    pub fn new_located(statements: &[List], location: Option<SourceLocation>) -> Self {
        ModuleBlock {
            statements: statements.to_vec(),
            location,
        }
    }
}

fundamental_primitive!(pub module_block for ModuleBlock);
#[derive(Clone)]
pub struct Module {
    pub env: EnvironmentRef,
}

impl Module {
    pub fn new(env: EnvironmentRef) -> Self {
        Module { env }
    }
}

fundamental_primitive!(pub module for Module);

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

            let block = Value::of(Block::new_located(
                &module_block.statements,
                module_block.location.clone(),
            ));

            let expanded_block = block
                .macro_expand(parameter, replacement, env, stack)?
                .get_primitive::<Block>(env, stack)?;

            Ok(Value::of(ModuleBlock::new_located(
                &expanded_block.statements,
                expanded_block.location,
            )))
        })
    });
}
