use crate::FilePath;
use internment::Intern;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::{hash::Hash, ops::Range};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub path: FilePath,
    pub primary: (usize, usize),
    pub expanded_from_operator: bool,
    pub caller: Option<(usize, usize)>,
}

impl Span {
    pub fn new(path: FilePath, range: Range<usize>) -> Self {
        Span {
            path,
            primary: (range.start, range.end),
            expanded_from_operator: false,
            caller: None,
        }
    }

    pub fn builtin() -> Self {
        Span::new(FilePath::Builtin, 0..0)
    }

    pub fn join(left: Span, right: Span) -> Self {
        Span {
            path: left.path,
            primary: (left.primary_start(), right.primary_end()),
            expanded_from_operator: false,
            caller: None,
        }
    }

    pub fn primary_start(self) -> usize {
        self.primary.0
    }

    pub fn primary_end(self) -> usize {
        self.primary.1
    }

    pub fn primary_range(self) -> Range<usize> {
        self.primary_start()..self.primary_end()
    }

    pub fn caller_start(self) -> Option<usize> {
        self.caller.map(|(start, _)| start)
    }

    pub fn caller_end(self) -> Option<usize> {
        self.caller.map(|(_, end)| end)
    }

    pub fn caller_range(self) -> Option<Range<usize>> {
        Some(self.caller_start()?..self.caller_end()?)
    }

    pub fn is_subspan_of(self, other: Span) -> bool {
        self.path == other.path
            && self.primary_start() >= other.primary_start()
            && self.primary_end() <= other.primary_end()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SpanList {
    first: Span,
    sources: Intern<SpanSources>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
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

    pub fn set_expanded_from_operator(&mut self) {
        self.first.expanded_from_operator = true;
    }

    pub fn set_caller(&mut self, caller: SpanList) {
        if self.first.path == caller.first.path {
            self.first.caller = Some((caller.first.primary_start(), caller.first.primary_end()));
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
