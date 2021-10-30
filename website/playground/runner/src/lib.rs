use serde::Serialize;
use std::{path::PathBuf, sync::Arc};
use wasm_bindgen::prelude::*;
use wipple_diagnostics::*;
use wipple_interpreter_backend::ExternalFunctions;
use wipple_parser::LocalIntern;

#[derive(Serialize)]
struct Result {
    output: Vec<String>,
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

    let value = wipple_parser::parse(path, code, &mut diagnostics)
        .and_then(|file| wipple_frontend::compile(file, &mut diagnostics))
        .map(|item| wipple_interpreter_backend::eval(item, external));

    // TODO: 'show' external function
    let output  = vec![
        String::from("I'm still working on playground support for 'show'. In the meantime, here is the value of the last statement in the program:\n"),
        format!("{:#?}", value),
    ];

    let result = Result {
        output,
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}
