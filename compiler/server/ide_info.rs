use serde::Deserialize;
use serde_json::json;

use crate::{InputMetadata, libraries::fetch_library};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    #[serde(flatten)]
    metadata: InputMetadata,
}

pub async fn handle(request: Request) -> anyhow::Result<serde_json::Value> {
    let mut info = Vec::new();
    let mut current = request.metadata.library;
    while let Some(library) = current {
        let library = fetch_library(&library).await?;

        if let Some(item) = library.metadata.ide {
            info.push(item);
        }

        current = library.metadata.library;
    }

    Ok(json!({ "info": info }))
}
