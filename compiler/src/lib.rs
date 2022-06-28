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
use std::{borrow::Cow, fmt};

pub trait Loader {
    type Error: fmt::Display;

    fn load(
        &mut self,
        path: FilePath,
        current: Option<FilePath>,
    ) -> Result<(FilePath, Cow<'static, str>), Self::Error>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FilePath {
    Path(InternedString),
    Virtual(InternedString),
    Prelude,
    Builtin(InternedString),
}

impl FilePath {
    pub fn as_str(&self) -> Cow<'static, str> {
        match self {
            FilePath::Path(path) => Cow::Borrowed(path.as_str()),
            FilePath::Virtual(name) => Cow::Borrowed(name.as_str()),
            FilePath::Prelude => Cow::Borrowed("prelude"),
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
pub struct Compiler<L> {
    loader: L,
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

            impl<L> Compiler<L> {
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

impl<L> Compiler<L> {
    pub fn new(loader: L) -> Self {
        Compiler {
            loader,
            diagnostics: Default::default(),
            ids: Default::default(),
        }
    }

    pub fn finish(self) -> (L, Diagnostics) {
        (self.loader, self.diagnostics)
    }
}
