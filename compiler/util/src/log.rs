/// Log to the JavaScript console.
#[macro_export]
#[deprecated(note = "use of `log!`")]
macro_rules! log {
    ($($args:tt)*) => {{
        if cfg!(target_arch = "wasm32") {
            $crate::web_sys::console::log_1(&format!($($args)*).into());
        }
    }};
}
