use serde::Serialize;
use std::{path::PathBuf, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_interpreter_backend::ExternalFunctions;
use wipple_parser::LocalIntern;

#[derive(Serialize)]
struct Result {
    output: String,
    diagnostics: Vec<Diagnostic>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    wipple_frontend::id::reset();

    let path = LocalIntern::new(PathBuf::from("playground.wpl"));

    let mut diagnostics = Diagnostics::new();
    diagnostics.add_file(path, Arc::from(code));

    let external = ExternalFunctions::new();

    // TODO: 'show' external function
    let output = String::new();

    if let Some(item) = wipple_parser::parse(path, code, &mut diagnostics)
        .and_then(|file| wipple_frontend::compile(file, &mut diagnostics))
    {
        wipple_interpreter_backend::eval(&item, external);
    }

    let result = Result {
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}
