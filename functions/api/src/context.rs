use crate::libraries::fetch_library;
use dashmap::DashMap;
use std::sync::{Arc, OnceLock};
use wipple_compiler::{Compiler, File, render::RenderedDiagnostic};

pub enum CompileError {
    UnsupportedLibrary(String, String),
    LibraryNotCompiled(String),
}

#[derive(Clone, Default)]
pub struct Context {
    pub cache: Arc<DashMap<String, Compiler>>,
    pub db: Option<mongodb::Database>,
}

static SHARED: OnceLock<Context> = OnceLock::new();

impl Context {
    pub fn set_shared(this: Self) -> anyhow::Result<()> {
        SHARED
            .set(this)
            .map_err(|_| anyhow::format_err!("context already initialized"))
    }

    pub fn shared() -> &'static Self {
        SHARED.get().expect("context not initialized")
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
        let (library_entry, cached) = fetch_library(name)
            .await
            .map_err(|e| CompileError::UnsupportedLibrary(name.to_string(), e.to_string()))?;

        if cached {
            if let Some(cached_compiler) = self.cache.get(name) {
                return Ok(cached_compiler.clone());
            }
        }

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
