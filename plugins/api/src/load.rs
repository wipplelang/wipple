#![cfg(not(target_arch = "wasm32"))]

use dlopen::symbor::Library;
use std::result::Result;
use wipple::*;

pub type PluginFn = extern "C" fn(&EnvironmentRef, &Stack) -> Box<wipple::Result>;

ref_thread_local! {
    // Libraries will be kept loaded until the program terminates
    static managed LOADED_LIBRARIES: Vec<Library> = Vec::new();
}

pub fn load_plugin(
    path: std::path::PathBuf,
    env: &EnvironmentRef,
    stack: &Stack,
) -> wipple::Result {
    let convert_error = |error| Return::error(&format!("Error loading plugin: {}", error), &stack);

    let lib = Library::open(path).map_err(convert_error)?;
    let plugin: PluginFn = *unsafe { lib.symbol("_wipple_plugin") }.map_err(convert_error)?;

    LOADED_LIBRARIES.borrow_mut().push(lib);

    // SAFETY: Need to bind to a variable to prevent premature deallocation of
    // the returned result
    let result = plugin(env, stack);

    *result
}
