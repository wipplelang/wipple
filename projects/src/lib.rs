mod dependencies;
mod load;
mod new;
mod project;
mod resolve;

use std::path::PathBuf;

pub use dependencies::*;
pub use load::*;
pub use new::*;
pub use project::*;
pub use resolve::*;

use wipple::*;

#[cfg(not(target_arch = "wasm32"))]
use wipple_plugins::*;

pub fn setup() {
    let env = Environment::global();

    env.borrow_mut().set_variable(
        "import",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .evaluate(env, stack)?
                .get_or::<Text>("Expected a path to a file or folder", env, stack)?
                .text;

            let module = import(&path, stack)?;

            Ok(Value::of(module))
        })),
    );

    #[cfg(not(target_arch = "wasm32"))]
    env.borrow_mut().set_variable(
        "load-plugin!",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .evaluate(env, stack)?
                .get_or::<Text>("Expected a path to a .wplplugin file", env, stack)?
                .text;

            let path = PathBuf::from(path);

            if !path.exists() {
                return Err(ReturnState::Error(Error::new(
                    &format!("Cannot find plugin at '{}'", path.to_string_lossy()),
                    stack,
                )));
            }

            load_plugin(path, env, stack)
        })),
    );

    // Text == Module
    // FIXME: This is impure and should not be a direct conformance; use some
    // kind of 'Import' trait instead
    env.borrow_mut()
        .add_conformance(Trait::text(), Trait::module(), |value, _, stack| {
            let text = value.clone().into_primitive::<Text>();

            let module = import(&text.text, stack)?;
            Ok(Value::of(module))
        });
}
