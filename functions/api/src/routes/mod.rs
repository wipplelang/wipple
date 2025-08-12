pub mod compile;
pub mod documentation;
pub mod format;
pub mod get_shared;
pub mod ide_info;
pub mod share;

use anyhow::Error;
use serde::Deserialize;
use serde_json::Value;

#[allow(async_fn_in_trait)]
pub trait Handle: Sized {
    type Response;

    async fn response(self) -> Result<Self::Response, Error>;

    async fn json_response(self) -> Result<Value, Error>
    where
        Self::Response: serde::Serialize,
    {
        let response = self.response().await?;
        Ok(serde_json::to_value(response)?)
    }
}

#[derive(Debug, Clone, Default, Hash, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct InputMetadata {
    pub library: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Request {
    Compile(compile::CompileRequest),
    Documentation(documentation::DocumentationRequest),
    Format(format::FormatRequest),
    GetShared(get_shared::GetSharedRequest),
    IdeInfo(ide_info::IdeInfoRequest),
    Share(share::ShareRequest),
}

impl Request {
    pub async fn response(self) -> Result<Value, Error> {
        match self {
            Request::Compile(req) => req.json_response().await,
            Request::Documentation(req) => req.json_response().await,
            Request::Format(req) => req.json_response().await,
            Request::GetShared(req) => req.json_response().await,
            Request::IdeInfo(req) => req.json_response().await,
            Request::Share(req) => req.json_response().await,
        }
    }
}
