pub mod analysis;
pub mod diagnostics;
pub mod helpers;
pub mod ir;
pub mod parse;

use async_trait::async_trait;
use diagnostics::*;
use helpers::InternedString;
use parking_lot::Mutex;
use serde::Serialize;
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{self, Debug},
    hash::Hash,
    mem,
    sync::{atomic::AtomicUsize, Arc},
};

pub type SourceMap = HashMap<FilePath, Arc<str>>;

#[async_trait]
pub trait Loader: Debug + Send + Sync + 'static {
    fn std_path(&self) -> Option<FilePath>;

    fn resolve(&self, path: FilePath, current: Option<FilePath>) -> anyhow::Result<FilePath>;

    async fn load(&self, path: FilePath) -> anyhow::Result<Arc<str>>;

    fn cache(&self) -> Arc<Mutex<HashMap<FilePath, Arc<analysis::expand::File>>>>;

    fn source_map(&self) -> Arc<Mutex<SourceMap>>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum FilePath {
    Path(InternedString),
    Url(InternedString),
    Virtual(InternedString),
    Builtin(InternedString),
}

impl FilePath {
    pub fn as_str(&self) -> Cow<'static, str> {
        match self {
            FilePath::Path(path) => Cow::Borrowed(path.as_str()),
            FilePath::Url(url) => Cow::Borrowed(url.as_str()),
            FilePath::Virtual(name) => Cow::Borrowed(name.as_str()),
            FilePath::Builtin(location) => Cow::Owned(format!("builtin ({})", location)),
        }
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct Compiler<'l> {
    loader: &'l dyn Loader,
    #[cfg(debug_assertions)]
    pub(crate) backtrace_enabled: bool,
    diagnostics: Diagnostics,
    file_ids: FileIds,
    ids: Ids,
    pub(crate) cache: Arc<Mutex<indexmap::IndexMap<FilePath, Arc<analysis::lower::File>>>>,
}

macro_rules! file_ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct FileIds {
                $([<next_ $id:snake>]: Arc<Mutex<HashMap<FilePath, usize>>>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
                pub struct $id {
                    pub file: FilePath,
                    pub counter: usize,
                }
            )*

            impl FileIds {
                $(
                    fn [<new_ $id:snake>](&self, file: FilePath) -> $id {
                        let mut storage = self.[<next_ $id:snake>].lock();
                        let storage = storage.entry(file).or_default();
                        let counter = *storage;
                        *storage += 1;

                        $id { file, counter }
                    }
                )*
            }

            impl Compiler<'_> {
                $(
                    fn [<new_ $id:snake>](&self, file: FilePath) -> $id {
                        self.file_ids.[<new_ $id:snake>](file)
                    }
                )*
            }
        }
    };
}

macro_rules! ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct Ids {
                $([<next_ $id:snake>]: Arc<AtomicUsize>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
                pub struct $id {
                    pub counter: usize,
                }
            )*

            impl Ids {
                $(
                    fn [<new_ $id:snake>](&self) -> $id {
                        let counter = self.[<next_ $id:snake>]
                            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                        $id { counter }
                    }
                )*
            }

            impl Compiler<'_> {
                $(
                    fn [<new_ $id:snake>](&self) -> $id {
                        self.ids.[<new_ $id:snake>]()
                    }
                )*
            }
        }
    };
}

file_ids!(
    BuiltinTypeId,
    GenericConstantId,
    ItemId,
    TemplateId,
    TraitId,
    TypeId,
    TypeParameterId,
    VariableId,
);

ids!(Label);

impl<'l> Compiler<'l> {
    pub fn new(loader: &'l impl Loader) -> Self {
        Compiler {
            loader,
            #[cfg(debug_assertions)]
            backtrace_enabled: false,
            diagnostics: Default::default(),
            ids: Default::default(),
            file_ids: Default::default(),
            cache: Default::default(),
        }
    }

    #[cfg(debug_assertions)]
    pub fn set_backtrace_enabled(mut self, backtrace_enabled: bool) -> Self {
        self.backtrace_enabled = backtrace_enabled;
        self
    }

    pub fn finish(&self) -> FinalizedDiagnostics {
        FinalizedDiagnostics {
            source_map: self.loader.source_map().lock().clone(),
            diagnostics: mem::take(&mut self.diagnostics.diagnostics.lock()),
        }
    }
}
