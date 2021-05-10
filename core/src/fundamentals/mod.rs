mod conformance;
mod diagnostics;
mod environment;
mod stack;
mod r#trait;
mod validation;
mod value;

pub use conformance::*;
pub use diagnostics::*;
pub use environment::*;
pub use r#trait::*;
pub use stack::*;
pub use validation::*;
pub use value::*;

pub type Result<T = Value> = std::result::Result<T, Return>;
