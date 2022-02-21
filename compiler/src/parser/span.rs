use crate::helpers::InternedString;
use serde::Serialize;
use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub file: InternedString,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: InternedString, range: Range<usize>) -> Self {
        Span {
            file,
            start: range.start,
            end: range.end,
        }
    }

    pub fn join(left: Span, right: Span) -> Self {
        Span::new(left.file, left.start..left.end).with_end(right.end)
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
        write!(f, "{} @ {}..{}", self.file, self.start, self.end)
    }
}
