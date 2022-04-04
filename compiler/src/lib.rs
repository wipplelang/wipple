pub mod build;
pub mod compile;
pub mod diagnostics;
pub mod helpers;
pub mod parser;

use diagnostics::*;
use helpers::InternedString;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt};

pub trait Loader {
    type Error: fmt::Display;

    fn load(&self, path: FilePath) -> Result<Cow<'static, str>, Self::Error>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FilePath {
    Path(InternedString),
    Virtual(InternedString),
    Prelude,
    _Builtin,
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilePath::Path(path) => f.write_str(path),
            FilePath::Virtual(name) => f.write_str(name),
            FilePath::Prelude => f.write_str("prelude"),
            FilePath::_Builtin => f.write_str("builtin"),
        }
    }
}

#[derive(Debug)]
pub struct Compiler<L: Loader> {
    loader: L,
    options: CompilerOptions,
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
                pub struct $id(usize);
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

            impl<L: Loader> Compiler<L> {
                $(
                    fn [<new_ $id:snake>](&mut self) -> $id {
                        self.ids.[<new_ $id:snake>]()
                    }
                )*
            }
        }
    };
}

ids! {
    TypeId,
    TypeParameterId,
    OperatorId,
    VariableId,
    ConstantId,
}

impl<L: Loader> Compiler<L> {
    pub fn new(loader: L, options: CompilerOptions) -> Self {
        let mut ids = Ids::default();

        Compiler {
            loader,
            options,
            diagnostics: Default::default(),
            ids,
        }
    }

    pub fn finish(self) -> Diagnostics {
        self.diagnostics
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
pub struct CompilerOptions {
    #[cfg_attr(feature = "clap", clap(long))]
    pub warn_unused_variables: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            warn_unused_variables: false,
        }
    }
}
