use crate::{helpers::InternedString, FilePath};
use internment::Intern;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::{hash::Hash, ops::Range};
use wipple_syntax::CharIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub path: FilePath,
    pub primary: (CharIndex, CharIndex),
    pub expanded_from_operator: Option<(
        InternedString,
        Option<Intern<(SpanList, Vec<SpanList>)>>,
        Option<Intern<(SpanList, Vec<SpanList>)>>,
    )>,
    pub caller: Option<(CharIndex, CharIndex)>,
}

impl Span {
    pub fn new(path: FilePath, range: Range<CharIndex>) -> Self {
        Span {
            path,
            primary: (range.start, range.end),
            expanded_from_operator: None,
            caller: None,
        }
    }

    pub fn builtin() -> Self {
        Span::new(FilePath::Builtin, CharIndex::ZERO..CharIndex::ZERO)
    }

    pub fn join(left: Span, right: Span) -> Self {
        let primary = if right
            .primary_end()
            .utf8
            .saturating_sub(left.primary_start().utf8)
            > (left.primary_end().utf8 - left.primary_start().utf8)
        {
            (left.primary_start(), right.primary_end())
        } else {
            left.primary
        };

        Span {
            path: left.path,
            primary,
            expanded_from_operator: None,
            caller: None,
        }
    }

    pub fn primary_start(self) -> CharIndex {
        self.primary.0
    }

    pub fn primary_end(self) -> CharIndex {
        self.primary.1
    }

    pub fn primary_range(self) -> Range<CharIndex> {
        self.primary_start()..self.primary_end()
    }

    pub fn caller_start(self) -> Option<CharIndex> {
        self.caller.map(|(start, _)| start)
    }

    pub fn caller_end(self) -> Option<CharIndex> {
        self.caller.map(|(_, end)| end)
    }

    pub fn caller_range(self) -> Option<Range<CharIndex>> {
        Some(self.caller_start()?..self.caller_end()?)
    }

    pub fn is_subspan_of(self, other: Span) -> bool {
        self.path == other.path
            && self.primary_start().utf8 >= other.primary_start().utf8
            && self.primary_end().utf8 <= other.primary_end().utf8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SpanList {
    first: Intern<Span>,
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
        *self.first
    }

    pub fn original(self) -> Span {
        self.sources.0.lock().last().copied().unwrap_or(*self.first)
    }

    pub fn split_iter(self) -> (Span, impl Iterator<Item = Span>) {
        // Can't return a reference to `self.sources.0.lock()`
        #[allow(clippy::unnecessary_to_owned)]
        (
            *self.first,
            self.sources.0.lock().to_vec().into_iter().rev(),
        )
    }

    pub fn join(left: SpanList, right: SpanList) -> SpanList {
        SpanList {
            first: Intern::new(Span::join(*left.first, *right.first)),
            sources: left.sources,
        }
    }

    pub fn set_expanded_from_operator(
        &mut self,
        name: InternedString,
        left: Option<(SpanList, Vec<SpanList>)>,
        right: Option<(SpanList, Vec<SpanList>)>,
    ) {
        let mut first = *self.first;
        first.expanded_from_operator = Some((name, left.map(Intern::new), right.map(Intern::new)));
        self.first = Intern::new(first);
    }

    pub fn set_caller(&mut self, caller: SpanList) {
        if self.first.path == caller.first.path {
            let mut first = *self.first;
            first.caller = Some((caller.first.primary_start(), caller.first.primary_end()));
            self.first = Intern::new(first);
        }
    }

    #[must_use]
    pub fn merge(self, span: SpanList) -> SpanList {
        let sources = std::iter::once(*self.first)
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
            first: Intern::new(span),
            sources: Default::default(),
        }
    }
}
