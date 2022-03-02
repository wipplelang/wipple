use crate::{diagnostics::*, parser};

mod lower;
mod typecheck;

pub use typecheck::*;

pub fn compile(file: parser::File, diagnostics: &mut Diagnostics) -> Option<File> {
    lower::lower(file, diagnostics).map(|file| typecheck::typecheck(&file, diagnostics))
}
