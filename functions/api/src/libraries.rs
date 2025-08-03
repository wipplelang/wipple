use dashmap::DashMap;
use http_cache_reqwest::{Cache, HttpCache, HttpCacheOptions, MokaManager};
use reqwest::Client;
use reqwest_middleware::{ClientBuilder, ClientWithMiddleware};
use serde::Deserialize;
use std::{env, sync::LazyLock};
use wipple_compiler::File;

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Library {
    pub metadata: LibraryMetadata,
    pub files: Vec<File>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LibraryMetadata {
    pub library: Option<String>,
    pub ide: Option<serde_json::Value>,
}

static CLIENT: LazyLock<ClientWithMiddleware> = LazyLock::new(|| {
    ClientBuilder::new(Client::new())
        .with(Cache(HttpCache {
            mode: http_cache_reqwest::CacheMode::Default,
            manager: MokaManager::default(),
            options: HttpCacheOptions::default(),
        }))
        .build()
});

static CACHE: LazyLock<DashMap<String, Library>> = LazyLock::new(DashMap::new);

pub async fn fetch_library(name: &str) -> anyhow::Result<(Library, bool)> {
    let library_url = env::var("LIBRARY_URL")?;

    let response = CLIENT
        .get(format!("{library_url}/{name}.json"))
        .send()
        .await?
        .error_for_status()?;

    let is_cached = response
        .headers()
        .get("x-cache")
        .is_some_and(|value| value == "HIT");

    if is_cached {
        if let Some(cached) = CACHE.get(name) {
            return Ok((cached.clone(), true));
        }
    }

    let library: Library = response.json().await?;

    CACHE.insert(name.to_string(), library.clone());

    Ok((library, false))
}
