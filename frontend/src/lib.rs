//! Compile Wipple programs into bytecode.

#![warn(missing_docs)]

#[macro_use]
mod utils;

pub mod diagnostics;
pub mod parser;
pub mod resolve;
