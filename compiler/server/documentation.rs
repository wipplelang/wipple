use crate::{InputMetadata, compile_library};
use serde::Deserialize;
use serde_json::json;
use wipple::{feedback::FeedbackWriter, queries};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Request {
    #[serde(flatten)]
    metadata: InputMetadata,
    name: String,
}

pub async fn handle(request: Request) -> anyhow::Result<serde_json::Value> {
    let library_name = request
        .metadata
        .library
        .ok_or_else(|| anyhow::format_err!("missing library"))?;

    let db = compile_library(&library_name).await?;

    let Some(data) = queries::find(&db, queries::documentation, |data| {
        data.name.as_deref() == Some(&request.name)
    }) else {
        return Ok(json!({ "documentation": null }));
    };

    let mut docs = String::new();
    FeedbackWriter::new(&db, &mut docs).write_comments(&data.comments);

    Ok(json!({
        "documentation": {
            "declaration": data.declaration,
            "docs": docs,
        },
    }))
}
