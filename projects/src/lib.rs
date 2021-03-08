mod load;
mod project;
mod resolve;

pub use load::*;
pub use project::*;
pub use resolve::*;

use wipple::*;

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

    env.borrow_mut()
        .add_conformance(TraitID::module(), |value, env, stack| {
            let text = match value.get_primitive_if_present::<Text>(env, stack)? {
                Some(text) => text,
                None => return Ok(None),
            };

            let module = import(&text.text, stack)?;
            Ok(Some(Value::of(module)))
        });
}
