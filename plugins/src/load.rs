#![cfg(not(target_arch = "wasm32"))]

use dlopen::symbor::Library;
use wipple::*;

pub type PluginFn = extern "C" fn(&EnvironmentRef, &Stack) -> Box<Result>;

pub fn load_plugin(
    path: std::path::PathBuf,
    env: &wipple::EnvironmentRef,
    stack: &wipple::Stack,
) -> wipple::Result {
    let convert_error = |error| {
        wipple::ReturnState::Error(wipple::Error::new(
            &format!("Error loading plugin: {}", error),
            &stack,
        ))
    };

    let lib = Library::open(path).map_err(convert_error)?;
    let plugin: PluginFn = *unsafe { lib.symbol("_wipple_plugin") }.map_err(convert_error)?;

    // SAFETY: Need to bind to a variable to prevent premature deallocation of
    // the returned result
    let result = plugin(env, stack);

    *result
}
