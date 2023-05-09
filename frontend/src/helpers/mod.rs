mod interned_string;

pub mod did_you_mean;

use crate::Compiler;
pub use indexmap::{IndexMap, IndexSet};
pub use interned_string::*;
pub use wipple_util::*;

impl Compiler {
    pub(crate) fn backtrace(&self) -> Backtrace {
        Backtrace {
            #[cfg(debug_assertions)]
            trace: self
                .backtrace_enabled
                .then(backtrace::Backtrace::new_unresolved)
                .map(Shared::new),
        }
    }
}
