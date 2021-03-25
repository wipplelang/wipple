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

pub fn setup() {
    let env = Environment::global();

    env.borrow_mut().set_variable(
        "import",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .get_primitive_or::<Text>(
                    "Expected a path to a file or folder",
                    env,
                    stack.clone(),
                )?
                .text;

            let module = import(&path, stack)?;

            Ok(Value::of(module))
        })),
    );

    env.borrow_mut().set_variable(
        "load-plugin!",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .get_primitive_or::<Text>(
                    "Expected a path to a folder containing .wplplugin files",
                    env,
                    stack.clone(),
                )?
                .text;

            let path = resolve_plugin(&path, stack.clone())?;

            load_plugin(path, env, stack)
        })),
    );

    // Text ::= Module
    env.borrow_mut()
        .add_conformance(ID::module(), |value, env, stack| {
            let text = match value.get_primitive_if_present::<Text>(env, stack.clone())? {
                Some(text) => text,
                None => return Ok(None),
            };

            let module = import(&text.text, stack)?;
            Ok(Some(Value::of(module)))
        });
}
