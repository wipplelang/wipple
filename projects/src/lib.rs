mod load;
mod new;
mod project;
mod resolve;

pub use load::*;
pub use new::*;
pub use project::*;
pub use resolve::*;

pub use wipple_loading::*;

use wipple::*;
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

    env.borrow_mut().set_variable(
        "load-plugin!",
        Value::of(Function::new(|value, env, stack| {
            let path_string = value
                .evaluate(env, stack)?
                .get_or::<Text>("Expected a path to a .wplplugin file", env, stack)?
                .text;

            let path = resolve(&path_string, stack)?;

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
