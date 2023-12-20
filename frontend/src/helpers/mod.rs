mod interned_string;

pub mod did_you_mean;

use crate::Compiler;
pub use indexmap::{IndexMap, IndexSet};
pub use interned_string::*;
pub use wipple_util::*;

impl Compiler {
    pub(crate) fn backtrace(&self) -> Backtrace {
        #[cfg(debug_assertions)]
        if self.backtrace_enabled {
            return Backtrace::capture();
        }

        Backtrace::empty()
    }
}
