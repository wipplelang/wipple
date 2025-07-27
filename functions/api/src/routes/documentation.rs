use crate::{context::Context, routes::InputMetadata};
use anyhow::Error;
use serde::{Deserialize, Serialize};
use wipple_compiler::render::RenderedDocumentation;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct DocumentationRequest {
    #[serde(flatten)]
    pub metadata: InputMetadata,
    pub name: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct DocumentationResponse {
    pub documentation: Option<RenderedDocumentation>,
}

impl super::Handle for DocumentationRequest {
    type Response = DocumentationResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        let Some(library_name) = self.metadata.library.as_deref() else {
            return Err(anyhow::format_err!("missing library"));
        };

        let documentation = Context::shared()
            .compile_library(library_name)
            .await
            .ok()
            .and_then(|compiler| compiler.documentation(&self.name));

        Ok(DocumentationResponse { documentation })
    }
}
