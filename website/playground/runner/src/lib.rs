use serde::Serialize;
use std::{path::PathBuf, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

#[derive(Serialize)]
pub struct Result {
    pub diagnostics: Vec<Diagnostic>,
    pub item: Option<wipple_frontend::lower::SpannedItem>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let path = LocalIntern::new(PathBuf::from("playground.wpl"));

    let mut diagnostics = Diagnostics::new();
    diagnostics.add_file(path, Arc::from(code));

    let item = wipple_parser::parse(path, code, &mut diagnostics)
        .map(|file| wipple_frontend::lower::lower(file, &mut diagnostics));

    let result = Result {
        diagnostics: diagnostics.diagnostics,
        item,
    };

    JsValue::from_serde(&result).unwrap()
}
