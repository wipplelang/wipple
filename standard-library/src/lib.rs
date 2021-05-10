mod r#loop;
mod prelude;
mod show;

pub use r#loop::*;
pub use show::*;

use wipple::*;
use wipple_plugins::wipple_plugin;

#[derive(rust_embed::RustEmbed)]
#[folder = "lib"]
struct Asset;

#[wipple_plugin]
pub fn setup(env: &Environment, stack: &Stack) -> Result {
    prelude::prelude(env);
    show::setup(env);
    r#loop::setup(env);

    for path in Asset::iter() {
        let file = Asset::get(&path).unwrap();
        let code = std::str::from_utf8(file.as_ref()).unwrap();
        let program = wipple_loading::load_string(code, None, stack)?;

        // Instead of running the files directly in the parent environment, run
        // each file in its own child environment and then 'use' it to prevent
        // private/local items from leaking (this is the same behavior as
        // regular Wipple code)

        let module = wipple_loading::import_program_with_parent_env(program, None, &env, stack)?;

        env.borrow_mut().r#use(&module.env.borrow());
    }

    Ok(Value::empty())
}
