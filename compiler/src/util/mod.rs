//! Shared utilities for the compiler.

pub use lazy_static;

mod info;
pub use info::*;

mod word_boundary;
pub use word_boundary::*;
