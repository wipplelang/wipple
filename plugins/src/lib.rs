use std::path::PathBuf;

use libloading::*;
use wipple::*;

#[macro_export]
macro_rules! wipple_plugin {
    ($func:expr) => {
        #[no_mangle]
        pub extern "C" fn _wipple_plugin(env: &EnvironmentRef, stack: &Stack) -> Box<Result> {
            Box::new($func(env, stack))
        }
    };
}

pub fn load_plugin(path: PathBuf, env: &EnvironmentRef, stack: &Stack) -> Result {
    let convert_error = |error| {
        wipple::ReturnState::Error(wipple::Error::new(
            &format!("Error loading plugin: {}", error),
            stack,
        ))
    };

    let lib = unsafe { Library::new(path) }.map_err(convert_error)?;

    let plugin: Symbol<extern "C" fn(&EnvironmentRef, &Stack) -> Box<Result>> =
        unsafe { lib.get(b"_wipple_plugin\0") }.map_err(convert_error)?;

    let result = plugin(env, stack);

    *result
}
