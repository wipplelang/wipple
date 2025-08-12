use crate::context::Context;
use anyhow::Error;
use mongodb::bson::doc;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct GetSharedRequest {
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetSharedResponse {
    pub runtime: String,
    pub code: String,
}

impl super::Handle for GetSharedRequest {
    type Response = GetSharedResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        let db = Context::shared()
            .db
            .as_ref()
            .ok_or_else(|| anyhow::format_err!("sharing not enabled"))?;

        let collection = db.collection("playgrounds");

        collection
            .find_one(doc! {"_id": self.id})
            .await?
            .ok_or_else(|| anyhow::format_err!("playground not found"))
    }
}
