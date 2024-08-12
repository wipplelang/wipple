//! Shared utilities for the compiler.

pub use lazy_static;

#[cfg(not(target_arch = "wasm32"))]
mod binary;
#[cfg(not(target_arch = "wasm32"))]
pub use binary::*;

mod info;
pub use info::*;

mod path;
pub use path::*;
