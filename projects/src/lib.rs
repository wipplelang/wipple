mod load;
mod new;
mod project;
mod resolve;

pub use load::*;
pub use new::*;
pub use project::*;
pub use resolve::*;

use wipple::*;

#[cfg(feature = "plugins")]
use wipple_plugins::*;

pub fn setup() {
    let env = Environment::global();

    env.borrow_mut().set_variable(
        "import",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .get_primitive_or::<Text>("Expected a path to a file or folder", env, stack)?
                .text;

            let module = import(&path, stack)?;

            Ok(Value::of(module))
        })),
    );

    #[cfg(feature = "plugins")]
    env.borrow_mut().set_variable(
        "load-plugin!",
        Value::of(Function::new(|value, env, stack| {
            let path = value
                .get_primitive_or::<Text>(
                    "Expected a path to a folder containing .wplplugin files",
                    env,
                    stack,
                )?
                .text;

            let path = resolve_plugin(&path, stack)?;

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
