pub mod conformance;
pub mod environment;
pub mod error;
#[macro_use]
pub mod r#trait;
pub mod validation;
pub mod value;

pub use conformance::*;
pub use environment::*;
pub use error::*;
pub use r#trait::*;
pub use validation::*;
pub use value::*;

pub type Result<T = Value> = std::result::Result<T, ProgramError>;
