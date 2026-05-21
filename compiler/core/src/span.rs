use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::fmt::Display;

pub type Str = SmolStr;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Span {
    pub path: Str,
    pub start: Location,
    pub end: Location,
    pub source: Str,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

impl Span {
    pub fn new_in(path: Str, start: Location, end: Location, source: Str) -> Self {
        let source = Str::new(&source[start.index..start.index.max(end.index)]);

        Span {
            path,
            start,
            end,
            source,
        }
    }

    pub fn empty() -> Self {
        Span::new_in(
            Str::new_static(""),
            Location::empty(),
            Location::empty(),
            Str::new_static(""),
        )
    }

    pub fn join_in(&self, other: &Self, source: Str) -> Self {
        Span::new_in(
            self.path.clone(),
            self.start.clone(),
            other.end.clone(),
            source,
        )
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}-{}:{}",
            self.path, self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}

impl Location {
    pub fn empty() -> Self {
        Location {
            line: 1,
            column: 1,
            index: 0,
        }
    }
}
