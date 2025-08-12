use crate::context::Context;
use anyhow::Error;
use mongodb::bson::doc;
use nanoid::nanoid;
use serde::{Deserialize, Serialize};
use std::sync::atomic::AtomicBool;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct ShareRequest {
    pub runtime: String,
    pub code: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct ShareResponse {
    pub id: String,
}

const EXPIRATION: u64 = 7 * 86400; // 7 days

impl super::Handle for ShareRequest {
    type Response = ShareResponse;

    async fn response(self) -> Result<Self::Response, Error> {
        static INIT: AtomicBool = AtomicBool::new(false);

        let db = Context::shared()
            .db
            .as_ref()
            .ok_or_else(|| anyhow::format_err!("sharing not enabled"))?;

        let collection = db.collection("playgrounds");

        // Expire documents
        if !INIT.swap(true, std::sync::atomic::Ordering::Relaxed) {
            collection
                .create_index(
                    mongodb::IndexModel::builder()
                        .keys(doc! { "createdAt": 1 })
                        .options(
                            mongodb::options::IndexOptions::builder()
                                .expire_after(std::time::Duration::from_secs(EXPIRATION))
                                .build(),
                        )
                        .build(),
                )
                .await?;
        }

        let id = nanoid!(6);

        collection
            .insert_one(doc! {
                "_id": id.as_str(),
                "createdAt": mongodb::bson::DateTime::now(),
                "runtime": self.runtime,
                "code": self.code,
            })
            .await?;

        Ok(ShareResponse { id })
    }
}
