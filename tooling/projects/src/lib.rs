mod load;
mod new;
mod project;
mod resolve;

pub use load::*;
pub use new::*;
pub use project::*;
pub use resolve::*;

use wipple::*;
use wipple_plugins::*;
use wipple_stdlib::*;

#[ext(pub, name = EnvProjectsExt)]
impl Env {
    fn projects(stack: &Stack) -> Result<Env> {
        Env::try_with(|env| {
            env.set_variable(
                "import",
                Value::of(Function::new(|value, env, stack| {
                    let value = value.evaluate(env, stack)?;
                    let path =
                        value.get_or::<Text>("Expected a path to a file or folder", env, stack)?;

                    let module = import(&path.text, stack)?;

                    Ok(Value::of(module))
                })),
            );

            env.set_variable(
                "include!",
                Value::of(Function::new(|value, env, stack| {
                    let value = value.evaluate(env, stack)?;
                    let path = value.get_or::<Text>("Expected a path to a file", env, stack)?;
                    include(&path.text, env, stack)
                })),
            );

            env.set_variable(
                "plugin!",
                Value::of(Function::new(|value, env, stack| {
                    let value = value.evaluate(env, stack)?;

                    let path = value.get_or::<Text>(
                        "Expected a path to a .wplplugin file, including the extension",
                        env,
                        stack,
                    )?;

                    let path = resolve(&path.text, stack)?;

                    load_plugin(&path, env, stack)
                })),
            );

            // Text == Module
            // FIXME: This is impure and should not be a direct relation; use some
            // kind of 'Import' trait instead
            env.add_relation(
                Trait::of::<Text>(),
                Trait::of::<Module>(),
                stack,
                DeriveValueFn::new(|value, _, stack| {
                    let text = value.into_primitive().unwrap().into_cast::<Text>();
                    let module = import(&text.text, stack)?;
                    Ok(Value::of(module))
                }),
            )?;

            Ok(())
        })
    }
}
