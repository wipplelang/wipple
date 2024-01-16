//! Shared utilities for the compiler.

pub use web_sys;

#[macro_use]
mod log;

mod info;
mod traverse;

pub use info::*;
pub use traverse::*;
