use crate::FilePath;
use serde::{Deserialize, Serialize};
use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub path: FilePath,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(path: FilePath, range: Range<usize>) -> Self {
        Span {
            path,
            start: range.start,
            end: range.end,
        }
    }

    pub fn join(left: Span, right: Span) -> Self {
        Span::new(left.path, left.start..left.end).with_end(right.end)
    }

    pub fn with_start(self, start: usize) -> Self {
        Span { start, ..self }
    }

    pub fn with_end(self, end: usize) -> Self {
        Span { end, ..self }
    }

    pub fn offset(self, range: usize) -> Self {
        Span {
            start: self.start + range,
            end: self.end + range,
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}..{}", self.path, self.start, self.end)
    }
}
