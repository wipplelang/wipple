mod conformance;
mod diagnostics;
mod environment;
mod pattern;
mod stack;
mod r#trait;
mod value;

pub use conformance::*;
pub use diagnostics::*;
pub use environment::*;
pub use pattern::*;
pub use r#trait::*;
pub use stack::*;
pub use value::*;

pub type Result<T = Value> = std::result::Result<T, Return>;
