#[cfg(not(target_arch = "wasm32"))]
use libloading::*;

#[macro_export]
macro_rules! wipple_plugin {
    ($func:expr) => {
        #[no_mangle]
        pub extern "C" fn _wipple_plugin(
            env: &wipple::EnvironmentRef,
            stack: wipple::Stack,
        ) -> Box<wipple::Result> {
            Box::new($func(env, stack))
        }
    };
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_plugin(
    path: std::path::PathBuf,
    env: &wipple::EnvironmentRef,
    stack: wipple::Stack,
) -> wipple::Result {
    let convert_error = |error| {
        wipple::ReturnState::Error(wipple::Error::new(
            &format!("Error loading plugin: {}", error),
            stack.clone(),
        ))
    };

    let lib = unsafe { Library::new(path) }.map_err(convert_error)?;

    let plugin: Symbol<
        extern "C" fn(&wipple::EnvironmentRef, wipple::Stack) -> Box<wipple::Result>,
    > = unsafe { lib.get(b"_wipple_plugin\0") }.map_err(convert_error)?;

    let result = plugin(env, stack);

    *result
}
