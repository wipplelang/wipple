pub mod analysis;
pub mod diagnostics;
pub mod doc;
pub mod helpers;
pub mod ir;
pub mod parse;

use async_trait::async_trait;
use diagnostics::*;
use helpers::InternedString;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt,
    hash::Hash,
    sync::{atomic::AtomicUsize, Arc},
};

pub type SourceMap = HashMap<FilePath, Arc<str>>;

#[async_trait]
pub trait Loader: Clone + Send + Sync + 'static {
    type Error: fmt::Display;

    fn std_path(&self) -> Option<FilePath>;

    fn resolve(&self, path: FilePath, current: Option<FilePath>) -> Result<FilePath, Self::Error>;

    async fn load(&self, path: FilePath) -> Result<Arc<str>, Self::Error>;

    fn cache(&self) -> Arc<Mutex<HashMap<FilePath, Arc<analysis::expand::File<Self>>>>>;

    fn source_map(&self) -> Arc<Mutex<SourceMap>>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
pub struct Compiler<L: Loader> {
    pub loader: L,
    diagnostics: Diagnostics,
    ids: Ids,
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
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::serde::Serialize, ::serde::Deserialize)]
                pub struct $id(pub usize);
            )*

            impl Ids {
                $(
                    fn [<new_ $id:snake>](&self) -> $id {
                        let id = self
                            .[<next_ $id:snake>]
                            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                        $id(id)
                    }
                )*
            }

            impl<L: Loader> Compiler<L> {
                $(
                    fn [<new_ $id:snake>](&self) -> $id {
                        self.ids.[<new_ $id:snake>]()
                    }
                )*
            }
        }
    };
}

ids!(
    BuiltinTypeId,
    IrComputationId,
    GenericConstantId,
    MonomorphizedConstantId,
    TemplateId,
    TraitId,
    TypeId,
    TypeParameterId,
    VariableId,
);

impl<L: Loader> Compiler<L> {
    pub fn new(loader: L) -> Self {
        Compiler {
            loader,
            diagnostics: Default::default(),
            ids: Default::default(),
        }
    }

    pub fn finish(self) -> FinalizedDiagnostics<L> {
        FinalizedDiagnostics {
            loader: self.loader,
            diagnostics: Arc::try_unwrap(self.diagnostics.diagnostics)
                .unwrap()
                .into_inner(),
        }
    }
}
