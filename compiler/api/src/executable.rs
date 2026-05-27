use crate::CompileResult;
use wasm_bindgen::prelude::*;
use wipple_core::codegen::{self, codegen, wasm};

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn executable(&self) -> Option<Box<[u8]>> {
        let program = codegen(&self.db, &self.statements, &self.lib_statements).ok()?;

        let wasm = wasm::to_bytes(
            &self.db,
            &program,
            codegen::Options {
                trace: codegen::TraceOptions::Files(&[&self.path]),
                ..Default::default()
            },
        )
        .ok()?;

        wasm::validate(&wasm).ok()?;

        Some(wasm.into_boxed_slice())
    }
}
