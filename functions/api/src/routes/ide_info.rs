use crate::{context::Context, routes::InputMetadata};
use anyhow::Error;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct IdeInfoRequest {
    #[serde(flatten)]
    pub metadata: InputMetadata,
}

#[derive(Debug, Clone, Serialize)]
pub struct IdeInfoResponse {
    pub info: Vec<Value>,
}

impl super::Handle for IdeInfoRequest {
    type Response = IdeInfoResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        let mut library = self.metadata.library.as_deref();
        let mut info = Vec::new();

        while let Some(next) = library {
            let Some(library_entry) = Context::shared().libraries.get(next) else {
                return Err(anyhow::format_err!("unsupported library: '{next}'"));
            };

            if let Some(info_entry) = &library_entry.metadata.ide {
                info.push(info_entry.clone());
            }

            library = library_entry.metadata.library.as_deref();
        }

        Ok(IdeInfoResponse { info })
    }
}
