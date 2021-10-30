#[macro_use]
pub mod id;
pub mod debug_info;
pub mod lower;
pub mod typecheck;

use wipple_diagnostics::Diagnostics;

pub fn compile(
    file: wipple_parser::File,
    diagnostics: &mut Diagnostics,
) -> Option<typecheck::Item> {
    let item = lower::lower(file, diagnostics);
    typecheck::typecheck(item, diagnostics)
}
