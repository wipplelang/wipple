use crate::FilePath;
use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn builtin() -> Self {
        Span::new(FilePath::Builtin, 0..0)
    }

    pub fn join(left: Span, right: Span) -> Self {
        left.with_end(right.end)
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

    pub fn is_subspan_of(self, other: Span) -> bool {
        self.path == other.path && self.start >= other.start && self.end <= other.end
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}..{}", self.path, self.start, self.end)
    }
}
