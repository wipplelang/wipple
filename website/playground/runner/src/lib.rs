use serde::Serialize;
use std::{path::PathBuf, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_parser::LocalIntern;

#[derive(Serialize)]
pub struct Result<T> {
    pub diagnostics: Vec<Diagnostic>,
    pub item: Option<T>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::id::reset();

    let path = LocalIntern::new(PathBuf::from("playground.wpl"));

    let mut diagnostics = Diagnostics::new();
    diagnostics.add_file(path, Arc::from(code));

    let item = wipple_parser::parse(path, code, &mut diagnostics)
        .and_then(|file| wipple_frontend::compile(file, &mut diagnostics));

    let result = Result {
        diagnostics: diagnostics.diagnostics,
        item,
    };

    JsValue::from_serde(&result).unwrap()
}
