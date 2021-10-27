pub mod id;
mod lower;
mod typecheck;

pub use id::*;
pub use lower::{info::Variable, item::*};

use wipple_diagnostics::Diagnostics;

pub fn compile(file: wipple_parser::File, diagnostics: &mut Diagnostics) -> Option<Item> {
    let item = lower::lower(file, diagnostics);
    typecheck::typecheck(item, diagnostics)
}
