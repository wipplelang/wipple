use serde::Deserialize;
use serde_json::json;
use wipple::syntax::format;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    code: String,
}

pub async fn handle(request: Request) -> anyhow::Result<serde_json::Value> {
    let formatted = format(&request.code).filter(|formatted| *formatted != request.code);

    Ok(json!({ "code": formatted }))
}
