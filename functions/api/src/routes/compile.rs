use crate::{
    context::{CompileError, Context},
    routes::InputMetadata,
};
use lambda_runtime::Error;
use serde::Deserialize;
use serde_json::{Value, json};
use wipple_compiler::File;

#[derive(Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct CompileRequest {
    #[serde(flatten)]
    metadata: InputMetadata,
    code: String,
}

pub async fn handle(req: CompileRequest) -> Result<Value, Error> {
    let file = File {
        path: String::from("input"),
        code: req.code.clone(),
    };

    let library_name = req.metadata.library.as_deref();

    let result = Context::shared()
        .compile(vec![file], library_name)
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
        Ok(compiler) => json!({
            "success": true,
            "executable": compiler
                .js_executable()
                .ok_or_else(|| anyhow::format_err!("could not create executable"))?,
        }),
        Err(diagnostics) => json!({
            "success": false,
            "diagnostics": diagnostics
        }),
    })
}
