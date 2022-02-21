pub mod compile;
pub mod diagnostics;
pub mod helpers;
pub mod parser;

use std::sync::Arc;

pub fn compile(file: &str, code: &str) -> (Option<compile::File>, diagnostics::Diagnostics) {
    let file = helpers::InternedString::new(file);

    let mut diagnostics = diagnostics::Diagnostics::new();
    diagnostics.add_file(file, Arc::from(code));

    let result = parser::parse(file, code, &mut diagnostics)
        .and_then(|file| compile::compile(&file, &mut diagnostics));

    (result, diagnostics)
}
