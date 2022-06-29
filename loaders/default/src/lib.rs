use path_clean::PathClean;
use std::{collections::HashMap, path::PathBuf, rc::Rc, str::FromStr, sync::Arc};
use url::Url;
use wipple_compiler::{compile, helpers::InternedString, FilePath, SourceMap};

pub const STD_URL: &str = "https://std.wipple.gramer.dev/std.wpl";

pub struct Loader {
    pub virtual_paths: HashMap<InternedString, Arc<str>>,
    fetcher: Fetcher,
    base: String,
    std_path: Option<FilePath>,
    source_map: SourceMap,
    cache: HashMap<FilePath, Rc<compile::expand::File<Self>>>,
}

pub struct Fetcher {
    from_path: Box<dyn Fn(&str) -> anyhow::Result<String>>,
    from_url: Box<dyn Fn(Url) -> anyhow::Result<String>>,
}

impl Fetcher {
    pub fn new(
        from_path: impl Fn(&str) -> anyhow::Result<String> + 'static,
        from_url: impl Fn(Url) -> anyhow::Result<String> + 'static,
    ) -> Self {
        Fetcher {
            from_path: Box::new(from_path),
            from_url: Box::new(from_url),
        }
    }
}

impl Loader {
    #[cfg(not(target_arch = "wasm32"))]
    pub fn new(base: String, std_path: Option<FilePath>) -> Self {
        Loader::with_fetcher(
            base,
            std_path,
            Fetcher::new(
                |path| std::fs::read_to_string(path).map_err(|e| e.into()),
                |url| reqwest::blocking::get(url)?.text().map_err(|e| e.into()),
            ),
        )
    }

    pub fn with_fetcher(base: String, std_path: Option<FilePath>, fetcher: Fetcher) -> Self {
        Loader {
            virtual_paths: Default::default(),
            fetcher,
            base,
            std_path,
            source_map: Default::default(),
            cache: Default::default(),
        }
    }
}

impl wipple_compiler::Loader for Loader {
    type Error = anyhow::Error;

    fn std_path(&self) -> Option<FilePath> {
        self.std_path
    }

    fn resolve(
        &mut self,
        path: FilePath,
        current: Option<FilePath>,
    ) -> Result<FilePath, Self::Error> {
        match path {
            FilePath::Path(path) => {
                if is_url(path) {
                    Ok(FilePath::Path(path))
                } else {
                    let base = match current {
                        Some(FilePath::Path(path)) => path.as_str(),
                        _ => &self.base,
                    };

                    let parsed_path = PathBuf::from(path.as_str());

                    if parsed_path.is_relative() {
                        match Url::from_str(base) {
                            Ok(base) => {
                                let url = base.join(path.as_str())?;
                                Ok(FilePath::Path(InternedString::from(url.to_string())))
                            }
                            Err(_) => {
                                let path = PathBuf::from(
                                    current
                                        .map(|path| path.to_string())
                                        .unwrap_or_else(|| self.base.to_string()),
                                )
                                .parent()
                                .unwrap()
                                .join(parsed_path)
                                .clean();

                                Ok(FilePath::Path(InternedString::new(path.to_str().unwrap())))
                            }
                        }
                    } else {
                        Ok(FilePath::Path(path))
                    }
                }
            }
            FilePath::Virtual(path) => Ok(FilePath::Virtual(path)),
            _ => unimplemented!(),
        }
    }

    fn load(&mut self, path: FilePath) -> Result<Arc<str>, Self::Error> {
        let code = match path {
            FilePath::Path(path) => Arc::from(match Url::from_str(&path) {
                Ok(url) => (self.fetcher.from_url)(url)?,
                Err(_) => (self.fetcher.from_path)(path.as_str())?,
            }),
            FilePath::Virtual(path) => self
                .virtual_paths
                .get(&path)
                .cloned()
                .ok_or_else(|| anyhow::Error::msg("invalid virtual path"))?,
            _ => unimplemented!(),
        };

        Ok(code)
    }

    fn cache(&mut self) -> &mut HashMap<FilePath, Rc<compile::expand::File<Self>>> {
        &mut self.cache
    }

    fn source_map(&mut self) -> &mut SourceMap {
        &mut self.source_map
    }
}

pub fn is_url(s: impl AsRef<str>) -> bool {
    Url::from_str(s.as_ref()).is_ok()
}
