#![warn(
    clippy::dbg_macro,
    clippy::todo,
    clippy::print_stdout,
    clippy::print_stderr
)]

#[macro_use]
mod macros;

pub mod analysis;
pub mod diagnostics;
pub mod helpers;
pub mod ir;

use async_trait::async_trait;
use diagnostics::*;
use helpers::{InternedString, Shared};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    hash::Hash,
    mem,
    sync::{atomic::AtomicUsize, Arc},
};

pub type SourceMap = HashMap<FilePath, Arc<str>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FileKind {
    Source,
    Plugin,
}

#[async_trait]
pub trait Loader: Debug + Send + Sync + 'static {
    fn std_path(&self) -> Option<FilePath>;

    fn resolve(
        &self,
        path: FilePath,
        kind: FileKind,
        current: Option<FilePath>,
    ) -> anyhow::Result<FilePath>;

    async fn load(&self, path: FilePath) -> anyhow::Result<Arc<str>>;

    async fn plugin(
        &self,
        path: FilePath,
        name: InternedString,
        input: PluginInput,
        api: &dyn PluginApi,
    ) -> anyhow::Result<PluginOutput>;

    fn virtual_paths(&self) -> Shared<HashMap<InternedString, Arc<str>>>;

    fn queue(&self) -> HashSet<FilePath>;

    fn cache(&self) -> Shared<HashMap<FilePath, Arc<analysis::ast::File<analysis::Analysis>>>>;

    fn source_map(&self) -> Shared<SourceMap>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum FilePath {
    Path(InternedString),
    Url(InternedString),
    Virtual(InternedString),
    Builtin,
}

impl FilePath {
    pub fn as_str(&self) -> &'static str {
        match self {
            FilePath::Path(path) => path.as_str(),
            FilePath::Url(url) => url.as_str(),
            FilePath::Virtual(name) => name.as_str(),
            FilePath::Builtin => "builtin",
        }
    }
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct PluginInput {
    id: ExpressionId,
    span: analysis::SpanList,
    ty: analysis::Type,
    inputs: Vec<analysis::Expression>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PluginOutput {
    pub expr: analysis::lower::Expression,
}

#[async_trait]
pub trait PluginApi: Send + Sync {
    // TODO
}

#[derive(Debug, Clone)]
pub struct Compiler {
    pub loader: Arc<dyn Loader>,
    diagnostics: Diagnostics,
    file_ids: FileIds,
    node_ids: NodeIds,
    ids: Ids,
    pub(crate) cache: Shared<indexmap::IndexMap<FilePath, Arc<analysis::lower::File>>>,
    #[cfg(debug_assertions)]
    pub(crate) backtrace_enabled: bool,
}

macro_rules! file_ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct FileIds {
                $([<next_ $id _id>]: Shared<HashMap<Option<FilePath>, usize>>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
                pub struct [<$id:camel Id>] {
                    pub file: Option<FilePath>,
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
                )*
            }

            #[allow(unused)]
            impl Compiler {
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
    scope,
    syntax,
    r#trait,
    r#type,
    type_parameter,
    variable,
    builtin_syntax,
);

macro_rules! node_ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct NodeIds {
                $([<next_ $id _id>]: Shared<HashMap<Option<ConstantId>, usize>>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
                pub struct [<$id:camel Id>] {
                    pub owner: Option<ConstantId>, // `None` if entrypoint
                    pub counter: usize,
                }
            )*

            #[allow(unused)]
            impl NodeIds {
                $(
                    fn [<new_ $id _id>](&self, owner: impl Into<Option<ConstantId>>) -> [<$id:camel Id>] {
                        let owner = owner.into();

                        let mut storage = self.[<next_ $id _id>].lock();
                        let storage = storage.entry(owner).or_default();
                        let counter = *storage;
                        *storage += 1;

                        [<$id:camel Id>] { owner, counter }
                    }
                )*
            }

            #[allow(unused)]
            impl Compiler {
                $(
                    fn [<new_ $id _id>](&self, owner: impl Into<Option<ConstantId>>) -> [<$id:camel Id>] {
                        self.node_ids.[<new_ $id _id>](owner)
                    }
                )*
            }
        }
    };
}

node_ids!(expression, pattern);

macro_rules! ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Clone, Default)]
            struct Ids {
                $([<next_ $id _id>]: Arc<AtomicUsize>,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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

            impl Compiler {
                $(
                    fn [<new_ $id _id>](&self) -> [<$id:camel Id>] {
                        self.ids.[<new_ $id _id>]()
                    }
                )*
            }
        }
    };
}

ids!(enumeration, structure, reachable_marker);

macro_rules! indexes {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
                pub struct [<$id:camel Index>](usize);

                impl [<$id:camel Index>] {
                    pub fn new(n: usize) -> Self {
                        [<$id:camel Index>](n)
                    }

                    pub fn into_inner(self) -> usize {
                        self.0
                    }
                }
            )*
        }
    };
}

indexes!(field, variant);

impl Compiler {
    pub fn new(loader: impl Loader) -> Self {
        Compiler {
            loader: Arc::new(loader),
            diagnostics: Default::default(),
            file_ids: Default::default(),
            node_ids: Default::default(),
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

impl Compiler {
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

impl Compiler {
    pub fn optimize_with<T: Optimize>(&self, x: T, options: T::Options) -> T {
        x.optimize(options, self)
    }
}
