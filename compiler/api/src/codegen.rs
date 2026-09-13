use crate::CompileResult;
use wasm_bindgen::prelude::*;
use wipple_core::codegen;

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn codegen(&self) -> Option<Vec<u8>> {
        let hir = codegen::hir::Program::from_statements(
            &self.db,
            &self.source_files,
            &self.statements,
            &self.lib_statements,
            Default::default(),
        )
        .ok()?;

        let mut mir = codegen::mir::Program::default();
        mir.extend_from_hir(
            &self.db,
            &hir,
            &mut Default::default(),
            codegen::mir::Options {
                trace: codegen::mir::TraceOptions::Files(&[&self.path]),
            },
        )
        .ok()?;

        let bytes = rmp_serde::to_vec(&mir).ok()?;

        Some(bytes)
    }
}
