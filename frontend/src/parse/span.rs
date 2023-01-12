use crate::{helpers::InternedString, FilePath};
use std::{fmt, ops::Range};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Span {
    pub path: FilePath,

    #[cfg_attr(
        feature = "arbitrary",
        arbitrary(with = |u: &mut arbitrary::Unstructured| u.int_in_range(0..=100))
    )]
    pub start: usize,

    #[cfg_attr(
        feature = "arbitrary",
        arbitrary(with = |u: &mut arbitrary::Unstructured| u.int_in_range(0..=100))
    )]
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

    pub fn builtin(location: impl AsRef<str>) -> Self {
        Span::new(FilePath::Builtin(InternedString::new(location)), 0..0)
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

    pub fn is_subspan_of(self, other: Span) -> bool {
        self.path == other.path && self.start >= other.start && self.end <= other.end
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} @ {}..{}", self.path, self.start, self.end)
    }
}
