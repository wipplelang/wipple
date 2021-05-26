use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use wipple_stdlib::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShownValue {
    pub input: String,
    pub output: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterpreterResult {
    pub success: bool,
    pub text: String,
}

#[wasm_bindgen]
pub fn run(code: &str) -> JsValue {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    let result = run_and_collect_output(code, |_, _| Ok(()))
        .into_iter()
        .map(|output| match output {
            RunOutput::Log(text) => InterpreterResult {
                success: true,
                text,
            },
            RunOutput::Error(error) => InterpreterResult {
                success: false,
                text: error.to_string(),
            },
        })
        .collect::<Vec<_>>();

    JsValue::from_serde(&result).unwrap()
}
