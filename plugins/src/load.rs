#![cfg(not(target_arch = "wasm32"))]

use libloading::*;

pub fn load_plugin(
    path: std::path::PathBuf,
    env: &wipple::EnvironmentRef,
    stack: wipple::Stack,
) -> wipple::Result {
    let convert_error = |error| {
        wipple::ReturnState::Error(wipple::Error::new(
            &format!("Error loading plugin: {}", error),
            &stack,
        ))
    };

    let lib = unsafe { Library::new(path) }.map_err(convert_error)?;

    let plugin: Symbol<extern "C" fn(&wipple::EnvironmentRef, wipple::Stack) -> wipple::Result> =
        unsafe { lib.get(b"_wipple_plugin\0") }.map_err(convert_error)?;

    plugin(env, stack)
}
