#![allow(clippy::type_complexity)]

use async_trait::async_trait;
use futures::future::BoxFuture;
use parking_lot::Mutex;
use path_clean::PathClean;
use std::{collections::HashMap, path::PathBuf, str::FromStr, sync::Arc};
use url::Url;
use wipple_frontend::{analysis, helpers::InternedString, FilePath, SourceMap};

pub const STD_URL: &str = "https://pkg.wipple.gramer.dev/std/std.wpl";

#[derive(Debug, Clone)]
pub struct Loader {
    pub virtual_paths: Arc<Mutex<HashMap<InternedString, Arc<str>>>>,
    fetcher: Arc<Mutex<Fetcher>>,
    base: Option<FilePath>,
    std_path: Option<FilePath>,
    source_map: Arc<Mutex<SourceMap>>,
    cache: Arc<Mutex<HashMap<FilePath, Arc<analysis::expand::File>>>>,
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
                                anyhow::Error::msg(format!(
                                    "failed to create cache directory: {}",
                                    e
                                ))
                            })?;

                        tokio::fs::write(cache_path, file.clone())
                            .await
                            .map_err(|e| {
                                anyhow::Error::msg(format!("failed to cache {}: {}", url, e))
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
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new(base: Option<FilePath>, std_path: Option<FilePath>) -> Self {
        Loader::new_with_fetcher(
            base,
            std_path,
            Fetcher::new()
                .with_default_path_handler()
                .with_default_url_handler(),
        )
    }

    pub fn new_with_fetcher(
        base: Option<FilePath>,
        std_path: Option<FilePath>,
        fetcher: Fetcher,
    ) -> Self {
        Loader {
            virtual_paths: Default::default(),
            fetcher: Arc::new(Mutex::new(fetcher)),
            base,
            std_path,
            source_map: Default::default(),
            cache: Default::default(),
        }
    }

    pub fn with_fetcher(&self, f: impl FnOnce(Fetcher) -> Fetcher) {
        replace_with::replace_with_or_default(&mut *self.fetcher.lock(), f);
    }
}

#[async_trait]
impl wipple_frontend::Loader for Loader {
    fn std_path(&self) -> Option<FilePath> {
        self.std_path
    }

    fn resolve(&self, path: FilePath, current: Option<FilePath>) -> anyhow::Result<FilePath> {
        let path = match path {
            FilePath::Path(path) => {
                if is_url(path) {
                    Ok(FilePath::Url(path))
                } else {
                    let base = match current {
                        Some(path) => path,
                        _ => match self.base {
                            Some(base) => base,
                            None => return Ok(FilePath::Path(path)),
                        },
                    };

                    let parsed_path = PathBuf::from(path.as_str());

                    if parsed_path.has_root() {
                        Ok(FilePath::Path(path))
                    } else {
                        match base {
                            FilePath::Url(url) => {
                                let url = Url::from_str(&url).unwrap().join(path.as_str())?;
                                Ok(FilePath::Url(InternedString::from(url.to_string())))
                            }
                            FilePath::Path(path) => {
                                let path = PathBuf::from(path.as_str())
                                    .parent()
                                    .unwrap()
                                    .join(parsed_path)
                                    .clean();

                                Ok(FilePath::Path(InternedString::new(path.to_str().unwrap())))
                            }
                            FilePath::Virtual(_) => {
                                let base = match self.base {
                                    Some(base) => base,
                                    None => unimplemented!(),
                                };

                                let path = PathBuf::from(base.as_str().as_ref())
                                    .join(parsed_path)
                                    .clean();

                                Ok(FilePath::Path(InternedString::new(path.to_str().unwrap())))
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
            }
            FilePath::Virtual(path) => Ok(FilePath::Virtual(path)),
            _ => unimplemented!(),
        };

        path
    }

    async fn load(&self, path: FilePath) -> anyhow::Result<Arc<str>> {
        let code = match path {
            FilePath::Path(path) => {
                let fut = self.fetcher.lock().from_path.as_ref().ok_or_else(|| {
                    anyhow::Error::msg("this environment does not support loading from the paths")
                })?(path.as_str());

                Arc::from(fut.await?)
            }
            FilePath::Url(url) => {
                let fut = self.fetcher.lock().from_url.as_ref().ok_or_else(|| {
                    anyhow::Error::msg("this environment does not support loading from URLs")
                })?(Url::from_str(&url).unwrap());

                Arc::from(fut.await?)
            }
            FilePath::Virtual(path) => self
                .virtual_paths
                .lock()
                .get(&path)
                .cloned()
                .ok_or_else(|| anyhow::Error::msg("invalid virtual path"))?,
            _ => unimplemented!(),
        };

        Ok(code)
    }

    fn cache(&self) -> Arc<Mutex<HashMap<FilePath, Arc<analysis::expand::File>>>> {
        self.cache.clone()
    }

    fn source_map(&self) -> Arc<Mutex<SourceMap>> {
        self.source_map.clone()
    }
}

pub fn is_url(s: impl AsRef<str>) -> bool {
    Url::from_str(s.as_ref()).is_ok()
}
