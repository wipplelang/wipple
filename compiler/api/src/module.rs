use crate::CompileResult;
use wasm_bindgen::prelude::*;
use wipple_core::codegen::{self, codegen, js};

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn module(&self) -> Option<String> {
        let program = codegen(
            &self.db,
            &self.source_files,
            &self.statements,
            &self.lib_statements,
        )
        .ok()?;

        let result = js::to_js(
            &self.db,
            &program,
            codegen::Options {
                file_name: "out",
                source_root: "",
                trace: codegen::TraceOptions::Files(&[&self.path]),
            },
        )
        .ok()?;

        Some(result.module)
    }
}
