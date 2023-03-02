use crate::FilePath;
use internment::Intern;
use parking_lot::Mutex;
use std::{fmt, hash::Hash, ops::Range};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpanList {
    first: Span,
    sources: Intern<SpanSources>,
}

#[derive(Debug, Default)]
struct SpanSources(Mutex<Vec<Span>>);

impl PartialEq for SpanSources {
    fn eq(&self, other: &Self) -> bool {
        *self.0.lock() == *other.0.lock()
    }
}

impl Eq for SpanSources {}

impl Hash for SpanSources {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.lock().hash(state);
    }
}

impl SpanList {
    pub fn first(self) -> Span {
        self.first
    }

    pub fn split_iter(self) -> (Span, impl Iterator<Item = Span>) {
        // Can't return a reference to `self.sources.0.lock()`
        #[allow(clippy::unnecessary_to_owned)]
        (self.first, self.sources.0.lock().to_vec().into_iter().rev())
    }

    pub fn join(left: SpanList, right: SpanList) -> SpanList {
        SpanList {
            first: Span::join(left.first, right.first),
            sources: left.sources,
        }
    }

    #[must_use]
    pub fn merge(self, span: SpanList) -> SpanList {
        let sources = std::iter::once(self.first)
            .chain(span.sources.0.lock().iter().copied())
            .collect::<Vec<_>>();

        SpanList {
            first: span.first,
            sources: Intern::new(SpanSources(Mutex::new(sources))),
        }
    }
}

impl From<Span> for SpanList {
    fn from(span: Span) -> Self {
        SpanList {
            first: span,
            sources: Default::default(),
        }
    }
}
