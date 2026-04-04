use crate::{InputMetadata, compile_library};
use serde::Deserialize;
use serde_json::json;
use std::collections::HashMap;
use wipple::{feedback::FeedbackWriter, queries};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    #[serde(flatten)]
    metadata: InputMetadata,
}

pub async fn handle(request: Request) -> anyhow::Result<serde_json::Value> {
    let library_name = request
        .metadata
        .library
        .ok_or_else(|| anyhow::format_err!("missing library"))?;

    let (db, _) = compile_library(&library_name).await?;

    let mut items = HashMap::new();
    for data in queries::all(&db, queries::documentation) {
        let Some(name) = data.name else {
            continue;
        };

        let mut docs = String::new();
        FeedbackWriter::new(&db, &mut docs).write_comments(&data.comments);

        items.insert(
            name,
            json!({
                "declaration": data.declaration,
                "kind": data.kind,
                "docs": docs,
            }),
        );
    }

    Ok(json!({ "items": items }))
}
