#![allow(clippy::type_complexity)]

use async_trait::async_trait;
use futures::future::BoxFuture;
use path_clean::PathClean;
use std::{collections::HashMap, path::PathBuf, str::FromStr, sync::Arc};
use url::Url;
use wipple_frontend::{
    analysis::{self, Analysis},
    helpers::{InternedString, Shared},
    FilePath, PluginApi, PluginInput, PluginOutput, SourceMap,
};

pub const STD_URL: &str = "https://pkg.wipple.dev/std/std.wpl";

#[derive(Debug, Clone)]
pub struct Loader {
    virtual_paths: Shared<HashMap<InternedString, Arc<str>>>,
    fetcher: Shared<Fetcher>,
    plugin_handler: Shared<PluginHandler>,
    base: Option<FilePath>,
    std_path: Option<FilePath>,
    source_map: Shared<SourceMap>,
    cache: Shared<HashMap<FilePath, Arc<analysis::ast::File<Analysis>>>>,
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

#[derive(Default)]
pub struct PluginHandler {
    from_path: Option<
        Box<dyn Fn(&str, PluginInput) -> BoxFuture<anyhow::Result<PluginOutput>> + Send + Sync>,
    >,
    from_url: Option<
        Box<
            dyn Fn(Url, PluginInput) -> BoxFuture<'static, anyhow::Result<PluginOutput>>
                + Send
                + Sync,
        >,
    >,
}

impl PluginHandler {
    pub fn new() -> Self {
        Default::default()
    }
}

impl std::fmt::Debug for PluginHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PluginHandler").finish()
    }
}

impl Loader {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new(base: Option<FilePath>, std_path: Option<FilePath>) -> Self {
        Loader {
            virtual_paths: Default::default(),
            fetcher: Default::default(),
            plugin_handler: Default::default(),
            base,
            std_path,
            source_map: Default::default(),
            cache: Default::default(),
        }
    }

    pub fn with_fetcher(mut self, fetcher: Fetcher) -> Self {
        self.fetcher = Shared::new(fetcher);
        self
    }

    pub fn with_plugin_handler(mut self, handler: PluginHandler) -> Self {
        self.plugin_handler = Shared::new(handler);
        self
    }
}

#[async_trait]
impl wipple_frontend::Loader for Loader {
    fn std_path(&self) -> Option<FilePath> {
        self.std_path
    }

    fn resolve(&self, path: FilePath, current: Option<FilePath>) -> anyhow::Result<FilePath> {
        fn set_extension_if_needed(path: &mut PathBuf) {
            if path.extension().is_none() {
                path.set_extension("wpl");
            }
        }

        match path {
            FilePath::Path(path) => {
                if is_url(path) {
                    let mut url = Url::from_str(&path)?.join(path.as_str())?;

                    let mut path = PathBuf::from(url.path());
                    set_extension_if_needed(&mut path);
                    url.set_path(path.to_str().unwrap());

                    Ok(FilePath::Url(InternedString::new(url.as_str())))
                } else {
                    let mut parsed_path = PathBuf::from(path.as_str());
                    set_extension_if_needed(&mut parsed_path);

                    if parsed_path.has_root() {
                        Ok(FilePath::Path(path))
                    } else {
                        let base = match current {
                            Some(path) => path,
                            None => match self.base {
                                Some(base) => base,
                                None => return Ok(FilePath::Path(path)),
                            },
                        };

                        match base {
                            FilePath::Url(base) => {
                                let mut url = Url::from_str(&base).unwrap().join(path.as_str())?;

                                let mut path = PathBuf::from(url.path());
                                set_extension_if_needed(&mut path);
                                url.set_path(path.to_str().unwrap());

                                Ok(FilePath::Url(InternedString::from(url.to_string())))
                            }
                            FilePath::Path(base) => {
                                let path = match PathBuf::from(base.as_str()).parent() {
                                    Some(base) => base.join(parsed_path).clean(),
                                    None => parsed_path,
                                };

                                Ok(FilePath::Path(InternedString::new(path.to_str().unwrap())))
                            }
                            FilePath::Virtual(_) => {
                                let base = match base {
                                    FilePath::Url(_) | FilePath::Path(_) => base,
                                    _ => self.base.ok_or_else(|| {
                                        anyhow::Error::msg(
                                            "attempt to load nested virtual path without base set",
                                        )
                                    })?,
                                };

                                match base {
                                    FilePath::Path(base) => {
                                        let path =
                                            PathBuf::from(base.as_str()).join(parsed_path).clean();

                                        Ok(FilePath::Path(InternedString::new(
                                            path.to_str().unwrap(),
                                        )))
                                    }
                                    FilePath::Url(base) => {
                                        let mut url = Url::from_str(&base)?.join(path.as_str())?;

                                        let mut path = PathBuf::from(url.path());
                                        set_extension_if_needed(&mut path);
                                        url.set_path(path.to_str().unwrap());

                                        Ok(FilePath::Url(InternedString::new(url.as_str())))
                                    }
                                    _ => Err(anyhow::Error::msg("base must be a file path or URL")),
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
            }
            FilePath::Virtual(path) => Ok(FilePath::Virtual(path)),
            _ => unimplemented!(),
        }
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

    async fn plugin(
        &self,
        name: &str,
        input: PluginInput,
        api: &dyn PluginApi,
    ) -> anyhow::Result<PluginOutput> {
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

    fn virtual_paths(&self) -> Shared<HashMap<InternedString, Arc<str>>> {
        self.virtual_paths.clone()
    }

    fn cache(&self) -> Shared<HashMap<FilePath, Arc<analysis::ast::File<Analysis>>>> {
        self.cache.clone()
    }

    fn source_map(&self) -> Shared<SourceMap> {
        self.source_map.clone()
    }
}

pub fn is_url(s: impl AsRef<str>) -> bool {
    Url::from_str(s.as_ref()).is_ok()
}
