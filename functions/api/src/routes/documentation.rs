use crate::{context::Context, routes::InputMetadata};
use lambda_runtime::Error;
use serde::Deserialize;
use serde_json::{Value, json};

#[derive(Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct DocumentationRequest {
    #[serde(flatten)]
    metadata: InputMetadata,
    name: String,
}

pub async fn handle(req: DocumentationRequest) -> Result<Value, Error> {
    let Some(library_name) = req.metadata.library.as_deref() else {
        return Err(anyhow::format_err!("missing library").into());
    };

    let documentation = Context::shared()
        .compile_library(library_name)
        .await
        .ok()
        .and_then(|compiler| compiler.documentation(&req.name));

    Ok(json!({
        "documentation": documentation,
    }))
}
