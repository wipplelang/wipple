use crate::File;
use dashmap::{DashMap, Entry};
use serde::Deserialize;
use std::{env, sync::LazyLock};

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Library {
    pub metadata: LibraryMetadata,
    pub files: Vec<File>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LibraryMetadata {
    pub library: Option<String>,
    pub ide: Option<serde_json::Value>,
}

static LIBRARIES: LazyLock<DashMap<String, Library>> = LazyLock::new(Default::default);

pub async fn fetch_library(name: &str) -> anyhow::Result<Library> {
    let entry = LIBRARIES.entry(name.to_string());

    let library = match entry {
        Entry::Occupied(entry) => entry.get().clone(),
        Entry::Vacant(entry) => {
            let library_url = env::var("LIBRARY_URL")?;

            let library = reqwest::get(format!("{library_url}/{name}.json"))
                .await?
                .error_for_status()?
                .json::<Library>()
                .await?;

            entry.insert(library).value().clone()
        }
    };

    Ok(library)
}
