pub mod show;

mod prelude;

use prelude::prelude;
use wipple::*;
use wipple_plugins::wipple_plugin;

#[derive(rust_embed::RustEmbed)]
#[folder = "lib"]
struct Asset;

wipple_plugin!(|env, stack: Stack| {
    prelude(env);
    show::setup(&mut env.borrow_mut());

    for path in Asset::iter() {
        let file = Asset::get(&path).unwrap();
        let code = std::str::from_utf8(file.as_ref()).unwrap();
        let program = wipple_projects::load_string(code, None, stack.clone())?;

        // Instead of running the files directly in the parent environment, run
        // each file in its own child environment and then 'use' it to prevent
        // private/local items from leaking

        let module =
            wipple_projects::import_program_with_parent_env(program, None, &env, stack.clone())?;

        env.borrow_mut().r#use(&module.env.borrow());
    }

    Ok(Value::empty())
});
