#![cfg(not(target_arch = "wasm32"))]

use dlopen::symbor::Library;
use std::path::Path;
use wipple::*;

pub type PluginFn = extern "C" fn(&Env, &Stack) -> Box<Result<Value>>;

thread_local! {
    // Libraries will be kept loaded until the program terminates
    static LOADED_LIBRARIES: Rc<RefCell<Vec<Library>>> = Default::default();
}

pub fn load_plugin(path: &Path, env: &Env, stack: &Stack) -> Result<Value> {
    let convert_error = |error| wipple::error(&format!("Error loading plugin: {}", error), &stack);

    let lib = Library::open(path).map_err(convert_error)?;
    let plugin: PluginFn = *unsafe { lib.symbol("_wipple_plugin") }.map_err(convert_error)?;

    LOADED_LIBRARIES.with(Clone::clone).borrow_mut().push(lib);

    // SAFETY: Need to bind to a variable to prevent premature deallocation of
    // the returned result
    let result = plugin(env, stack);

    *result
}
