//! Parse a source file into a syntax tree.

pub mod reader;

#[cfg(feature = "wipple-syntax")]
pub mod syntax;

mod grammar;
