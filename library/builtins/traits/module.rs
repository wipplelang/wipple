use crate::*;
use std::collections::HashMap;
use wipple::*;

#[derive(TypeInfo, Clone)]
pub struct Module {
    pub env: Env,
}

impl Primitive for Module {}

impl Module {
    pub fn new(env: Env) -> Self {
        Module { env }
    }
}

impl EvaluateBlockFn {
    pub fn into_module() -> Self {
        fn evaluate_block(block: &Block, env: &Env, stack: &Stack) -> Result<Value> {
            let env = env.child();
            env.set_evaluate_block(EvaluateBlockFn::into_module());

            block.reduce(&env, stack)?;
            Ok(Value::of(Module::new(env)))
        }

        EvaluateBlockFn::new(evaluate_block)
    }
}

pub(crate) fn setup(env: &Env, stack: &Stack) -> Result<()> {
    env.set_variable(stack, "Module", Value::of(Trait::of::<Module>()))?;

    // Module == Text
    env.add_text_relation::<Module>("module", stack)?;

    // Module == Function
    env.add_relation_between(stack, |module: Module| {
        Function::new(move |value, env, stack| {
            let name = value.get_or::<Name>("Expected a name", env, stack)?;
            name.resolve(&module.env, stack)
        })
    })?;

    // Module == Pattern
    env.add_relation_between_with(stack, |module: Module, stack| {
        let mut fields = HashMap::new();

        let variables = module.env.variables().0;
        for (name, variable) in variables {
            let pattern = variable
                .get_value(&module.env, stack)?
                .get_or::<Pattern>("Expected pattern", &module.env, stack)?
                .into_owned();

            fields.insert(name.clone(), pattern);
        }

        Ok(Pattern::new(move |value, env, stack| {
            let module = value
                .get_or::<Module>("Expected module", env, stack)?
                .into_owned();

            let validated_env = Env::new();

            let variables = module.env.variables().0;
            for (name, variable) in variables {
                let value = variable.get_value(env, stack)?;

                let validated = match fields.get(&name) {
                    Some(pattern) => pattern(&value, env, stack)?,
                    None => return Ok(None),
                };

                match validated {
                    Some(value) => validated_env.set_variable(stack, &name, value.into_owned())?,
                    None => return Ok(None),
                }
            }

            Ok(Some(Cow::Owned(Value::of(module))))
        }))
    })?;

    Ok(())
}
