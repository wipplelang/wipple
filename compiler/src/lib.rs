pub mod build;
pub mod compile;
pub mod diagnostics;
pub mod helpers;
pub mod parser;

use diagnostics::*;
use helpers::reset_ids;
use helpers::InternedString;
use serde::Serialize;
use std::{borrow::Cow, fmt};

pub trait Loader {
    type Error: fmt::Display;

    fn load(&self, path: FilePath) -> Result<Cow<'static, str>, Self::Error>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum FilePath {
    Path(InternedString),
    Virtual(&'static str),
    Prelude,
}

impl fmt::Display for FilePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilePath::Path(path) => f.write_str(path),
            FilePath::Virtual(name) => f.write_str(name),
            FilePath::Prelude => f.write_str("prelude"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Compiler<L: Loader> {
    loader: L,
    diagnostics: Diagnostics,
}

impl<L: Loader> Compiler<L> {
    pub fn new(loader: L) -> Self {
        reset_ids();

        Compiler {
            loader,
            diagnostics: Default::default(),
        }
    }

    pub fn finish(self) -> Diagnostics {
        self.diagnostics
    }
}
