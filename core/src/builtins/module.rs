use crate::*;
use std::collections::HashMap;

#[derive(TypeInfo, Clone)]
pub struct Module {
    pub env: Environment,
}

impl Module {
    pub fn new(env: Environment) -> Self {
        Module { env }
    }
}

impl EvaluateBlockFn {
    pub fn into_module() -> Self {
        fn evaluate_block(block: Block, env: &Environment, stack: &Stack) -> Result {
            let mut env = env::child_of(env);
            *env.evaluate_block() = EvaluateBlockFn::into_module();

            let env = env.into();

            block.reduce(&env, stack)?;
            Ok(Value::of(Module::new(env)))
        }

        EvaluateBlockFn::new(evaluate_block)
    }
}

pub(crate) fn setup(env: &mut EnvironmentInner) {
    env.set_variable("Module", Value::of(Trait::of::<Module>()));

    // Module == Text
    env.add_text_relation::<Module>("module");

    // Module == Function
    env.add_primitive_relation(|module: Module| {
        Function::new(move |value, env, stack| {
            let name = value.get_or::<Name>("Expected a name", env, stack)?;
            name.resolve(&module.env, stack)
        })
    });

    // Module == pattern
    env.add_relation(
        Pattern::for_trait(Trait::of::<Module>()),
        Trait::of::<Pattern>(),
        |value, env, stack| {
            let module = value.into_primitive::<Module>().unwrap();
            let variables = module.env.borrow_mut().variables().0.clone();

            let mut fields = HashMap::new();

            for (name, variable) in variables {
                let pattern = variable.get_value(env, stack)?.get_or::<Pattern>(
                    "Expected pattern",
                    env,
                    stack,
                )?;

                fields.insert(name, pattern);
            }

            Ok(Value::of(Pattern::new(move |value, env, stack| {
                let module = value.get_or::<Module>("Expected module", env, stack)?;

                let mut validated_env = env::blank();

                let variables = module.env.borrow_mut().variables().clone();
                for (name, variable) in variables.0 {
                    let value = variable.get_value(env, stack)?;

                    let validated = match fields.get(&name) {
                        Some(pattern) => pattern(value, env, stack)?,
                        None => return Ok(Validated::Invalid),
                    };

                    match validated.into_valid() {
                        Some(value) => validated_env.set_variable(&name, value),
                        None => return Ok(Validated::Invalid),
                    }
                }

                Ok(Validated::Valid(Value::of(module)))
            })))
        },
    );
}
