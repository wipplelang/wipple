use crate::{context::Context, routes::InputMetadata};
use lambda_runtime::Error;
use serde::Deserialize;
use serde_json::{Value, json};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct IdeInfoRequest {
    #[serde(flatten)]
    metadata: InputMetadata,
}

pub async fn handle(req: IdeInfoRequest) -> Result<Value, Error> {
    let mut library = req.metadata.library.as_deref();
    let mut info = Vec::new();

    while let Some(next) = library {
        let Some(library_entry) = Context::shared().libraries.get(next) else {
            return Err(anyhow::format_err!("unsupported library: '{next}'").into());
        };

        if let Some(info_entry) = &library_entry.metadata.ide {
            info.push(info_entry);
        }

        library = library_entry.metadata.library.as_deref();
    }

    Ok(json!({
        "info": info,
    }))
}
