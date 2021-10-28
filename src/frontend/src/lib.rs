pub mod id;
mod lower;
mod typecheck;

pub use id::*;
pub use lower::{info::Variable, item::*};
pub use typecheck::{Ty, TyKind, TypecheckedItem};

use wipple_diagnostics::Diagnostics;

pub fn compile(
    file: wipple_parser::File,
    diagnostics: &mut Diagnostics,
) -> Option<TypecheckedItem> {
    let item = lower::lower(file, diagnostics);
    typecheck::typecheck(item, diagnostics)
}
