pub mod compile;
pub mod diagnostics;
pub mod doc;
pub mod helpers;
pub mod lint;
pub mod optimize;
pub mod parse;

use diagnostics::*;
use helpers::InternedString;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::HashMap, fmt, hash::Hash, rc::Rc, sync::Arc};

pub type SourceMap = HashMap<FilePath, Arc<str>>;

pub trait Loader
where
    Self: Sized,
{
    type Error: fmt::Display;

    fn std_path(&self) -> Option<FilePath>;

    fn resolve(
        &mut self,
        path: FilePath,
        current: Option<FilePath>,
    ) -> Result<FilePath, Self::Error>;

    fn load(&mut self, path: FilePath) -> Result<Arc<str>, Self::Error>;

    fn cache(&mut self) -> &mut HashMap<FilePath, Rc<compile::expand::File<Self>>>;

    fn source_map(&mut self) -> &mut SourceMap;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FilePath {
    Path(InternedString),
    Virtual(InternedString),
    Builtin(InternedString),
}

impl FilePath {
    pub fn as_str(&self) -> Cow<'static, str> {
        match self {
            FilePath::Path(path) => Cow::Borrowed(path.as_str()),
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

#[derive(Debug)]
pub struct Compiler<'a, L> {
    pub loader: &'a mut L,
    diagnostics: Diagnostics,
    ids: Ids,
}

macro_rules! ids {
    ($($(#[$meta:meta])* $id:ident),* $(,)?) => {
        paste::paste! {
            #[derive(Debug, Default)]
            struct Ids {
                $([<next_ $id:snake>]: usize,)*
            }

            $(
                $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, ::serde::Serialize, ::serde::Deserialize)]
                pub struct $id(pub usize);
            )*

            impl Ids {
                $(
                    fn [<new_ $id:snake>](&mut self) -> $id {
                        let id = self.[<next_ $id:snake>];
                        self.[<next_ $id:snake>] += 1;
                        $id(id)
                    }
                )*
            }

            impl<L> Compiler<'_, L> {
                $(
                    fn [<new_ $id:snake>](&mut self) -> $id {
                        self.ids.[<new_ $id:snake>]()
                    }
                )*
            }
        }
    };
}

ids!(
    BuiltinTypeId,
    GenericConstantId,
    MonomorphizedConstantId,
    TemplateId,
    TraitId,
    TypeId,
    TypeParameterId,
    VariableId,
);

impl<'a, L> Compiler<'a, L> {
    pub fn new(loader: &'a mut L) -> Self {
        Compiler {
            loader,
            diagnostics: Default::default(),
            ids: Default::default(),
        }
    }

    pub fn finish(self) -> FinalizedDiagnostics<'a, L> {
        FinalizedDiagnostics {
            loader: self.loader,
            diagnostics: self.diagnostics.diagnostics,
        }
    }
}
