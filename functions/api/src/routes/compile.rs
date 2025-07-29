use crate::{
    context::{CompileError, Context},
    routes::InputMetadata,
};
use anyhow::Error;
use serde::{Deserialize, Serialize};
use wipple_compiler::{File, codegen, render::RenderedDiagnostic};

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct CompileRequest {
    #[serde(flatten)]
    pub metadata: InputMetadata,
    #[serde(flatten)]
    pub input: CompileRequestInput,
    #[serde(default)]
    pub js_options: codegen::js::Options,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum CompileRequestInput {
    Code(String),
    Files(Vec<File>),
}

#[derive(Debug, Clone, Serialize)]
#[serde(untagged)]
pub enum CompileResponse {
    Success {
        executable: String,
    },
    Failure {
        diagnostics: Vec<RenderedDiagnostic>,
    },
}

impl super::Handle for CompileRequest {
    type Response = CompileResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        let files = match self.input {
            CompileRequestInput::Code(code) => vec![File {
                path: String::from("input"),
                code,
            }],
            CompileRequestInput::Files(files) => files,
        };

        let library_name = self.metadata.library.as_deref();

        let result = Context::shared()
            .compile(files, library_name)
            .await
            .map_err(|error| match error {
                CompileError::UnsupportedLibrary(library) => {
                    anyhow::format_err!("unsupported library: '{library}'")
                }
                CompileError::LibraryNotCompiled(library) => {
                    anyhow::format_err!("library not compiled: '{library}'")
                }
            })?;

        Ok(match result {
            Ok(compiler) => CompileResponse::Success {
                executable: compiler
                    .js_executable(self.js_options)
                    .ok_or_else(|| anyhow::format_err!("could not create executable"))?,
            },
            Err(diagnostics) => CompileResponse::Failure { diagnostics },
        })
    }
}
