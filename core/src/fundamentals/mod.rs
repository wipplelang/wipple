mod conformances;
mod diagnostics;
mod environment;
mod traits;
mod values;

pub use conformances::*;
pub use diagnostics::*;
pub use environment::*;
pub use traits::*;
pub use values::*;

pub type Result<T = Value> = std::result::Result<T, ReturnState>;
