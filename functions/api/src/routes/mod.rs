mod compile;
mod documentation;
mod format;
mod ide_info;

use lambda_runtime::Error;
use serde::Deserialize;
use serde_json::Value;

#[derive(Default, Hash, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct InputMetadata {
    library: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Request {
    Compile(compile::CompileRequest),
    Documentation(documentation::DocumentationRequest),
    Format(format::FormatRequest),
    IdeInfo(ide_info::IdeInfoRequest),
}

pub async fn handle(req: Request) -> Result<Value, Error> {
    match req {
        Request::Compile(req) => compile::handle(req).await,
        Request::Documentation(req) => documentation::handle(req).await,
        Request::Format(req) => format::handle(req).await,
        Request::IdeInfo(req) => ide_info::handle(req).await,
    }
}
