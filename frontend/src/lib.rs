#[macro_use]
mod macros;

pub mod analysis;
pub mod diagnostics;
pub mod helpers;
pub mod ir;
pub mod parse;

use async_trait::async_trait;
use diagnostics::*;
use helpers::{InternedString, Shared};
use parse::Span;
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

    fn cache(&self) -> Shared<HashMap<FilePath, Arc<analysis::expand::File>>>;

    fn source_map(&self) -> Shared<SourceMap>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "value"))]
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

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for FilePath {
    fn arbitrary(_: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(FilePath::Virtual(InternedString::new("fuzz")))
    }
}

#[derive(Debug, Clone)]
pub struct Compiler<'l> {
    loader: &'l dyn Loader,
    diagnostics: Diagnostics,
    file_ids: FileIds,
    ids: Ids,
    pub(crate) cache: Shared<indexmap::IndexMap<FilePath, Arc<analysis::lower::File>>>,
    #[cfg(debug_assertions)]
    pub(crate) backtrace_enabled: bool,
}

#[cfg(feature = "arbitrary")]
pub(crate) const ARBITRARY_MAX_ID_COUNTER: usize = 4;

macro_rules! file_ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct FileIds {
                $([<next_ $id _id>]: Shared<HashMap<Option<FilePath>, usize>>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                #[cfg_attr(feature = "serde", derive(serde::Serialize), serde(transparent))]
                #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
                pub struct [<$id:camel Id>] {
                    #[cfg_attr(feature = "arbitrary", arbitrary(default))]
                    #[cfg_attr(feature = "serde", serde(skip))]
                    pub file: Option<FilePath>,

                    #[cfg_attr(
                        feature = "arbitrary",
                        arbitrary(with = |u: &mut arbitrary::Unstructured| {
                            u.choose_index(ARBITRARY_MAX_ID_COUNTER)
                        })
                    )]
                    pub counter: usize,
                }
            )*

            #[allow(unused)]
            impl FileIds {
                $(
                    fn [<new_ $id _id>](&self) -> [<$id:camel Id>] {
                        self.[<new_ $id _id_with>](None)
                    }

                    fn [<new_ $id _id_in>](&self, file: FilePath) -> [<$id:camel Id>] {
                        self.[<new_ $id _id_with>](Some(file))
                    }

                    fn [<new_ $id _id_with>](&self, file: Option<FilePath>) -> [<$id:camel Id>] {
                        let mut storage = self.[<next_ $id _id>].lock();
                        let storage = storage.entry(file).or_default();
                        let counter = *storage;
                        *storage += 1;

                        [<$id:camel Id>] { file, counter }
                    }

                    #[cfg(feature = "arbitrary")]
                    fn [<list_arbitrary_ $id _ids>]() -> impl Iterator<Item = [<$id:camel Id>]> {
                        (0..ARBITRARY_MAX_ID_COUNTER).map(|id| [<$id:camel Id>] {
                            file: None,
                            counter: id,
                        })
                    }

                    #[cfg(feature = "arbitrary")]
                    fn [<split_arbitrary_ $id _ids>]() -> (impl Iterator<Item = [<$id:camel Id>]>, impl Iterator<Item = [<$id:camel Id>]>) {
                        const SPLIT: usize = ARBITRARY_MAX_ID_COUNTER / 2;

                        (
                            (0..SPLIT).map(|id| [<$id:camel Id>] {
                                file: None,
                                counter: id,
                            }),
                            (SPLIT..ARBITRARY_MAX_ID_COUNTER).map(|id| [<$id:camel Id>] {
                                file: None,
                                counter: id,
                            }),
                        )
                    }
                )*
            }

            #[allow(unused)]
            impl Compiler<'_> {
                $(
                    fn [<new_ $id _id>](&self) -> [<$id:camel Id>] {
                        self.file_ids.[<new_ $id _id>]()
                    }

                    fn [<new_ $id _id_in>](&self, file: FilePath) -> [<$id:camel Id>] {
                        self.file_ids.[<new_ $id _id_in>](file)
                    }

                    fn [<new_ $id _id_with>](&self, file: Option<FilePath>) -> [<$id:camel Id>] {
                        self.file_ids.[<new_ $id _id_with>](file)
                    }
                )*
            }
        }
    };
}

file_ids!(
    builtin_type,
    constant,
    item,
    template,
    r#trait,
    r#type,
    type_parameter,
    variable,
);

macro_rules! ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
            struct Ids {
                $([<next_ $id _id>]: Arc<AtomicUsize>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                #[cfg_attr(feature = "serde", derive(serde::Serialize), serde(transparent))]
                #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
                pub struct [<$id:camel Id>] {
                    pub counter: usize,
                }
            )*

            impl Ids {
                $(
                    fn [<new_ $id _id>](&self) -> [<$id:camel Id>] {
                        let counter = self.[<next_ $id _id>]
                            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

                        [<$id:camel Id>] { counter }
                    }
                )*
            }

            impl Compiler<'_> {
                $(
                    fn [<new_ $id _id>](&self) -> [<$id:camel Id>] {
                        self.ids.[<new_ $id _id>]()
                    }
                )*
            }
        }
    };
}

ids!(enumeration, structure);

macro_rules! indexes {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
                #[cfg_attr(feature = "serde", derive(serde::Serialize), serde(transparent))]
                #[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
                pub struct [<$id:camel Index>](
                    #[cfg_attr(
                        feature = "arbitrary",
                        arbitrary(with = |u: &mut arbitrary::Unstructured| {
                            u.choose_index(ARBITRARY_MAX_ID_COUNTER)
                        })
                    )]
                    usize
                );

                impl [<$id:camel Index>] {
                    pub fn new(n: usize) -> Self {
                        [<$id:camel Index>](n)
                    }

                    pub fn into_inner(self) -> usize {
                        self.0
                    }

                    #[cfg(feature = "arbitrary")]
                    fn [<list_arbitrary>]() -> impl Iterator<Item = [<$id:camel Index>]> {
                        (0..ARBITRARY_MAX_ID_COUNTER).map([<$id:camel Index>]::new)
                    }
                }
            )*
        }
    };
}

indexes!(field, variant);

impl<'l> Compiler<'l> {
    pub fn new(loader: &'l impl Loader) -> Self {
        Compiler {
            loader,
            diagnostics: Default::default(),
            file_ids: Default::default(),
            ids: Default::default(),
            cache: Default::default(),
            #[cfg(debug_assertions)]
            backtrace_enabled: false,
        }
    }

    #[cfg(debug_assertions)]
    pub fn set_backtrace_enabled(mut self, backtrace_enabled: bool) -> Self {
        self.backtrace_enabled = backtrace_enabled;
        self
    }

    pub const DEFAULT_RECURSION_LIMIT: usize = 64;
}

impl<'l> Compiler<'l> {
    pub fn has_errors(&self) -> bool {
        self.diagnostics.contains_errors()
    }

    pub fn finish_analysis(&self) -> FinalizedDiagnostics {
        FinalizedDiagnostics {
            source_map: self.loader.source_map().lock().clone(),
            diagnostics: mem::take(&mut self.diagnostics.diagnostics.lock()),
        }
    }
}

pub trait Optimize {
    type Options;

    fn optimize(self, options: Self::Options, compiler: &Compiler) -> Self;
}

impl Compiler<'_> {
    pub fn optimize_with<T: Optimize>(&self, x: T, options: T::Options) -> T {
        x.optimize(options, self)
    }
}
