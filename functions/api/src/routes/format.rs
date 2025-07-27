use anyhow::Error;
use serde::{Deserialize, Serialize};
use wipple_compiler::Compiler;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct FormatRequest {
    code: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct FormatResponse {
    code: String,
}

impl super::Handle for FormatRequest {
    type Response = FormatResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        Ok(FormatResponse {
            code: Compiler::format(&self.code),
        })
    }
}
