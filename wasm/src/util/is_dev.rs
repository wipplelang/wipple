use crate::util::OrThrowExt;
use web_sys::js_sys;

pub fn is_dev() -> bool {
    js_sys::eval("typeof __DEV__ !== 'undefined' && __DEV__ === true")
        .or_throw("failed to get __DEV__")
        .as_bool()
        .or_throw("__DEV__ is not a boolean")
}
