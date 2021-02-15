pub mod conformance;
pub mod diagnostic;
pub mod environment;
#[macro_use]
pub mod r#trait;
pub mod validation;
pub mod value;

pub use conformance::*;
pub use diagnostic::*;
pub use environment::*;
pub use r#trait::*;
pub use validation::*;
pub use value::*;

pub type Result<T = Value> = std::result::Result<T, ProgramError>;
