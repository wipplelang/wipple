use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
struct Result {
    output: Vec<String>,
    diagnostics: Vec<wipple::diagnostics::Diagnostic>,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let (output, diagnostics) = wipple::compile("playground", code);

    let result = Result {
        output: output
            .map(|output| vec![format!("{output:#?}")])
            .unwrap_or_default(),
        diagnostics: diagnostics.diagnostics,
    };

    JsValue::from_serde(&result).unwrap()
}
