use crate::CompileResult;
use wasm_bindgen::prelude::*;
use wipple_core::codegen::{self, backends::Backend};

#[wasm_bindgen]
impl CompileResult {
    #[wasm_bindgen]
    pub fn module(&self) -> Option<String> {
        let hir = codegen::hir::Program::from_statements(
            &self.db,
            &self.source_files,
            &self.statements,
            &self.lib_statements,
            false,
        )
        .ok()?;

        let mir = codegen::mir::Program::from_hir(
            &self.db,
            &hir,
            codegen::mir::Options {
                trace: codegen::mir::TraceOptions::Files(&[&self.path]),
            },
        )
        .ok()?;

        let backend = codegen::backends::js::Backend::new(
            &self.db,
            codegen::backends::js::Options {
                file_name: None,
                source_root: "",
                include_prelude: true,
            },
        );

        let result = backend.run(&mir).ok()?;

        Some(result.module)
    }
}
