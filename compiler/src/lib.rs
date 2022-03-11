pub mod build;
pub mod compile;
pub mod diagnostics;
pub mod helpers;
pub mod parser;

use diagnostics::*;
use helpers::reset_ids;
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

#[derive(Debug, Default)]
pub struct Compiler<L: Loader> {
    loader: L,
    options: CompilerOptions,
    diagnostics: Diagnostics,
}

impl<L: Loader> Compiler<L> {
    pub fn new(loader: L, options: CompilerOptions) -> Self {
        reset_ids(); // TODO: Make IDs local to Compiler

        Compiler {
            loader,
            options,
            diagnostics: Default::default(),
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
