use crate::CompileResult;
use base64::prelude::*;
use wasm_bindgen::prelude::*;
use wipple_core::codegen::{self, codegen, wasm};

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn executable(&self) -> Option<String> {
        let program = codegen(&self.db, &self.statements, &self.lib_statements).ok()?;

        let wat = wasm::write_to_string(
            &self.db,
            &program,
            codegen::Options {
                trace: &[&self.path],
                ..Default::default()
            },
        )
        .ok()?;

        let wasm = wat::parse_str(wat).ok()?;

        Some(BASE64_STANDARD.encode(wasm))
    }
}
