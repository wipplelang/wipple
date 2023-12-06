#![allow(clippy::type_complexity)]
#![warn(clippy::dbg_macro, clippy::todo)]

use async_trait::async_trait;
use futures::future::BoxFuture;
use path_clean::PathClean;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};
use url::Url;
use wipple_frontend::{
    analysis::{self, Analysis},
    helpers::{InternedString, Shared},
    FilePath, SourceMap,
};

pub const STD_URL: &str = "https://wipple.dev/std/std.wpl";

pub fn make_example_url(example: &str) -> String {
    format!("https://wipple.dev/playground/?lesson=errors/{example}")
}

#[derive(Debug, Clone)]
pub struct Loader {
    pub virtual_paths: Shared<HashMap<InternedString, Arc<str>>>,
    fetcher: Shared<Fetcher>,
    base: Shared<Option<InternedString>>,
    std_path: Option<InternedString>,
    source_map: Shared<SourceMap>,
    queue: Shared<HashMap<InternedString, Arc<str>>>,
    cache: Shared<HashMap<InternedString, Arc<analysis::ast::File<Analysis>>>>,
}

#[derive(Default)]
pub struct Fetcher {
    from_path: Option<Box<dyn Fn(&str) -> BoxFuture<anyhow::Result<String>> + Send + Sync>>,
    from_url: Option<Box<dyn Fn(Url) -> BoxFuture<'static, anyhow::Result<String>> + Send + Sync>>,
}

impl Fetcher {
    pub fn new() -> Self {
        Default::default()
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn cache_dir() -> Option<PathBuf> {
        dirs::cache_dir().map(|dir| dir.join("wipple"))
    }

    pub fn with_path_handler(
        mut self,
        from_path: impl Fn(&str) -> BoxFuture<anyhow::Result<String>> + Send + Sync + 'static,
    ) -> Self {
        self.from_path = Some(Box::new(from_path));
        self
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn with_default_path_handler(self) -> Self {
        self.with_path_handler(|path| {
            Box::pin(async move { tokio::fs::read_to_string(path).await.map_err(|e| e.into()) })
        })
    }

    pub fn with_url_handler(
        mut self,
        from_url: impl Fn(Url) -> BoxFuture<'static, anyhow::Result<String>> + Send + Sync + 'static,
    ) -> Self {
        self.from_url = Some(Box::new(from_url));
        self
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn with_default_url_handler(self) -> Self {
        self.with_url_handler(|url| {
            Box::pin(async move {
                use sha2::{Digest, Sha256};

                let load = |cache_path: Option<PathBuf>| async {
                    let file = reqwest::get(url.clone()).await?.text().await?;

                    if let Some(cache_path) = cache_path {
                        tokio::fs::create_dir_all(cache_path.parent().unwrap())
                            .await
                            .map_err(|e| {
                                anyhow::Error::msg(format!("failed to create cache directory: {e}"))
                            })?;

                        tokio::fs::write(cache_path, file.clone())
                            .await
                            .map_err(|e| {
                                anyhow::Error::msg(format!("failed to cache {url}: {e}"))
                            })?;
                    }

                    Ok(file)
                };

                let cache_dir = match Self::cache_dir() {
                    Some(dir) => dir,
                    None => return load(None).await,
                };

                let hash = {
                    let mut hasher = Sha256::new();
                    hasher.update(url.as_str());
                    format!("{:x}", hasher.finalize())
                };

                let cache_path = cache_dir.join(hash);

                if !cache_path.exists() {
                    return load(Some(cache_path)).await;
                }

                let file = tokio::fs::read_to_string(cache_path).await?;

                Ok(file)
            })
        })
    }
}

impl std::fmt::Debug for Fetcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Fetcher").finish()
    }
}

impl Loader {
    pub fn new(base: Option<impl AsRef<str>>, std_path: Option<impl AsRef<str>>) -> Self {
        Loader {
            virtual_paths: Default::default(),
            fetcher: Default::default(),
            base: Shared::new(base.map(InternedString::new)),
            std_path: std_path.map(InternedString::new),
            source_map: Default::default(),
            queue: Default::default(),
            cache: Default::default(),
        }
    }

    pub fn set_base(&self, base: Option<impl AsRef<str>>) {
        *self.base.lock() = base.map(InternedString::new);
    }

    pub fn with_fetcher(&self, fetcher: Fetcher) -> Self {
        *self.fetcher.lock() = fetcher;
        self.clone()
    }
}

#[async_trait]
impl wipple_frontend::Loader for Loader {
    fn std_path(&self) -> Option<InternedString> {
        self.std_path
    }

    fn resolve(
        &self,
        path: InternedString,
        current: Option<InternedString>,
    ) -> anyhow::Result<InternedString> {
        fn set_extension_if_needed(path: &mut PathBuf) {
            if path.extension().is_none() {
                path.set_extension("wpl");
            }
        }

        if self.virtual_paths.lock().contains_key(&path) {
            return Ok(path);
        }

        if let Some(mut url) = Url::from_str(&path)
            .ok()
            .filter(|url| !url.cannot_be_a_base())
        {
            url = url.join(path.as_str())?;

            let mut path = PathBuf::from(url.path());
            set_extension_if_needed(&mut path);
            url.set_path(path.to_str().unwrap());

            return Ok(InternedString::new(url.as_str()));
        } else {
            let mut parsed_path = PathBuf::from(path.as_str());
            set_extension_if_needed(&mut parsed_path);

            if parsed_path.has_root() {
                Ok(InternedString::new(parsed_path.to_string_lossy()))
            } else {
                let base = match current {
                    Some(path) => path,
                    None => match *self.base.lock() {
                        Some(base) => base,
                        None => return Ok(InternedString::new(parsed_path.to_string_lossy())),
                    },
                };

                if let Some(mut url) = Url::from_str(&base)
                    .ok()
                    .filter(|url| !url.cannot_be_a_base())
                {
                    url = url.join(path.as_str())?;

                    let mut path = PathBuf::from(url.path());
                    set_extension_if_needed(&mut path);
                    url.set_path(path.to_str().unwrap());

                    Ok(InternedString::from(url.to_string()))
                } else {
                    let mut resolved_path = PathBuf::from(base.as_str());
                    if base.ends_with('/') {
                        resolved_path.push(path.as_str());
                    } else {
                        resolved_path.set_file_name(path.as_str());
                    }

                    resolved_path = resolved_path.clean();

                    set_extension_if_needed(&mut resolved_path);

                    Ok(InternedString::from(
                        resolved_path.into_os_string().into_string().unwrap(),
                    ))
                }
            }
        }
    }

    async fn load(&self, path: InternedString) -> anyhow::Result<Arc<str>> {
        if let Some(code) = self.queue.lock().get(&path) {
            return Ok(code.clone());
        }

        let virtual_code = self.virtual_paths.lock().get(&path).cloned();
        let (code, cache) = if let Some(code) = virtual_code {
            (code, false)
        } else if let Ok(url) = Url::from_str(path.as_str()) {
            let fut = self.fetcher.lock().from_url.as_ref().ok_or_else(|| {
                anyhow::Error::msg("this environment does not support loading from URLs")
            })?(url);

            (Arc::from(fut.await?), true)
        } else {
            let fut = self.fetcher.lock().from_path.as_ref().ok_or_else(|| {
                anyhow::Error::msg("this environment does not support loading from paths")
            })?(path.as_str());

            (Arc::from(fut.await?), true)
        };

        if cache {
            self.queue.lock().insert(path, code.clone());
        }

        Ok(code)
    }

    fn queue(&self) -> HashSet<InternedString> {
        self.queue.lock().keys().cloned().collect()
    }

    fn insert_virtual(&self, path: InternedString, code: Arc<str>) {
        self.virtual_paths.lock().insert(path, code);
    }

    fn cache(&self, path: InternedString, file: Arc<analysis::ast::File<analysis::Analysis>>) {
        if self.virtual_paths.lock().contains_key(&path) {
            return;
        }

        self.cache.lock().insert(path, file);
    }

    fn get_cached(
        &self,
        path: InternedString,
    ) -> Option<Arc<analysis::ast::File<analysis::Analysis>>> {
        self.cache.lock().get(&path).cloned()
    }

    fn source_map(&self) -> Shared<SourceMap> {
        self.source_map.clone()
    }
}

pub fn is_url(s: impl AsRef<str>) -> bool {
    Url::from_str(s.as_ref()).is_ok()
}

pub fn is_relative_to_entrypoint(path: FilePath, entrypoint: FilePath) -> bool {
    let (path, entrypoint) = match (path, entrypoint) {
        (FilePath::File(path), FilePath::File(entrypoint)) => (path, entrypoint),
        _ => return false,
    };

    match (
        Url::from_str(path.as_str()),
        Url::from_str(entrypoint.as_str()),
    ) {
        (Ok(url), Ok(entrypoint)) => {
            let path = PathBuf::from(url.path());

            let entrypoint_path = PathBuf::from(entrypoint.path());

            let entrypoint_dir = match entrypoint_path.parent() {
                Some(path) => path,
                None => return false,
            };

            url.origin() == entrypoint.origin() && path.strip_prefix(entrypoint_dir).is_ok()
        }
        (Err(_), Err(_)) => {
            let path = PathBuf::from(path.as_str());
            let entrypoint = PathBuf::from(entrypoint.as_str());

            let entrypoint_dir = match entrypoint.parent() {
                Some(path) => path,
                None => return false,
            };

            path.strip_prefix(entrypoint_dir).is_ok()
        }
        _ => false,
    }
}
