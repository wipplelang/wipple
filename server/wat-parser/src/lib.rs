use base64::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(wat: &str) -> Option<String> {
    let wasm = wat::parse_str(wat).ok()?;
    let base64 = BASE64_STANDARD.encode(wasm);
    Some(base64)
}
