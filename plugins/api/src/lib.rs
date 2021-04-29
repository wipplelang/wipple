mod load;
#[cfg(not(target_arch = "wasm32"))]
pub use load::*;

pub use wipple_plugin_attribute::wipple_plugin_attribute as wipple_plugin;
