use lambda_runtime::Error;
use serde::Deserialize;
use serde_json::{Value, json};
use wipple_compiler::Compiler;

#[derive(Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct FormatRequest {
    code: String,
}

pub async fn handle(req: FormatRequest) -> Result<Value, Error> {
    Ok(json!({
        "code": Compiler::format(&req.code),
    }))
}
