mod diagnostics;
mod environment;
mod pattern;
mod relation;
mod stack;
mod r#trait;
mod value;

pub use diagnostics::*;
pub use environment::*;
pub use pattern::*;
pub use r#trait::*;
pub use relation::*;
pub use stack::*;
pub use value::*;

pub type Result<T = Value> = std::result::Result<T, Return>;
