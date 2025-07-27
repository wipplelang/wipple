use crate::libraries::{LibraryEntry, load_libraries};
use dashmap::DashMap;
use std::{
    collections::HashMap,
    sync::{Arc, LazyLock},
};
use wipple_compiler::{Compiler, File, render::RenderedDiagnostic};

pub enum CompileError {
    UnsupportedLibrary(String),
    LibraryNotCompiled(String),
}

#[derive(Clone)]
pub struct Context {
    pub cache: Arc<DashMap<String, Compiler>>,
    pub libraries: Arc<HashMap<String, LibraryEntry>>,
}

impl Context {
    pub fn shared() -> &'static Self {
        static CONTEXT: LazyLock<Context> = LazyLock::new(Context::new);

        LazyLock::force(&CONTEXT)
    }

    fn new() -> Self {
        Context {
            cache: Default::default(),
            libraries: Arc::new(load_libraries()),
        }
    }

    pub async fn compile(
        &self,
        files: Vec<File>,
        library_name: Option<&str>,
    ) -> Result<Result<Compiler, Vec<RenderedDiagnostic>>, CompileError> {
        let mut compiler = match library_name {
            Some(library_name) => Box::pin(self.compile_library(library_name)).await?,
            None => Compiler::new(),
        };

        let diagnostics = compiler.compile(files);

        Ok(if diagnostics.is_empty() {
            Ok(compiler)
        } else {
            Err(diagnostics)
        })
    }

    pub async fn compile_library(&self, name: &str) -> Result<Compiler, CompileError> {
        if let Some(cached_compiler) = self.cache.get(name) {
            return Ok(cached_compiler.clone());
        }

        let library_entry = self
            .libraries
            .get(name)
            .ok_or_else(|| CompileError::UnsupportedLibrary(name.to_string()))?;

        let compiler = self
            .compile(
                library_entry.files.clone(),
                library_entry.metadata.library.as_deref(),
            )
            .await
            .ok()
            .and_then(|result| result.ok()) // ignore diagnostics
            .ok_or_else(|| CompileError::LibraryNotCompiled(name.to_string()))?;

        self.cache.insert(name.to_string(), compiler.clone());

        Ok(compiler)
    }
}
