use crate::{
    analysis::{Span, SpanList},
    helpers::{Backtrace, Shared},
    Compiler, FilePath, SourceMap,
};
use itertools::Itertools;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    ops::Range,
    sync::Arc,
};
use wipple_syntax::CharIndex;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub notes: Vec<Note>,
    pub fix: Option<Fix>,
    pub example: String,
    pub trace: Backtrace,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level
            && self.message == other.message
            && self.notes == other.notes
            && self.fix == other.fix
            && self.example == other.example
    }
}

impl Eq for Diagnostic {}

impl std::hash::Hash for Diagnostic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.level.hash(state);
        self.message.hash(state);
        self.notes.hash(state);
        self.fix.hash(state);
        self.example.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    Warning,
    Error,
}

impl From<DiagnosticLevel> for codespan_reporting::diagnostic::Severity {
    fn from(level: DiagnosticLevel) -> Self {
        match level {
            DiagnosticLevel::Warning => codespan_reporting::diagnostic::Severity::Warning,
            DiagnosticLevel::Error => codespan_reporting::diagnostic::Severity::Error,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Note {
    pub level: NoteLevel,
    pub span: SpanList,
    pub message: String,
    pub use_caller_if_available: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NoteLevel {
    Primary,
    Secondary,
}

impl From<NoteLevel> for codespan_reporting::diagnostic::LabelStyle {
    fn from(level: NoteLevel) -> Self {
        match level {
            NoteLevel::Primary => codespan_reporting::diagnostic::LabelStyle::Primary,
            NoteLevel::Secondary => codespan_reporting::diagnostic::LabelStyle::Secondary,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fix {
    pub description: String,
    pub range: FixRange,
    pub replacement: String,
}

impl Fix {
    pub fn new(description: impl ToString, range: FixRange, replacement: impl ToString) -> Self {
        Fix {
            description: description.to_string(),
            range,
            replacement: replacement.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FixRange(pub Range<CharIndex>);

impl FixRange {
    pub fn replace(span: Span) -> FixRange {
        FixRange(span.primary_range())
    }

    pub fn before(span: Span) -> FixRange {
        FixRange(span.primary_start()..span.primary_start())
    }

    pub fn after(span: Span) -> FixRange {
        FixRange(span.primary_end()..span.primary_end())
    }

    pub fn range(&self) -> Range<CharIndex> {
        self.0.clone()
    }
}

impl Diagnostic {
    pub fn fix(mut self, fix: impl Into<Option<Fix>>) -> Self {
        self.fix = fix.into();
        self
    }

    pub fn fix_with(
        self,
        description: impl ToString,
        range: FixRange,
        replacement: impl ToString,
    ) -> Self {
        self.fix(Fix::new(description, range, replacement))
    }
}

#[allow(unused)]
impl Compiler {
    #[must_use]
    pub(crate) fn diagnostic(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) -> Diagnostic {
        self.diagnostic_with_trace(level, message, notes, example, self.backtrace())
    }

    #[must_use]
    pub(crate) fn diagnostic_with_trace(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message: message.to_string(),
            notes,
            fix: None,
            example: example.to_string(),
            trace,
        }
    }

    #[must_use]
    pub(crate) fn warning(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) -> Diagnostic {
        self.diagnostic(DiagnosticLevel::Warning, message, notes, example)
    }

    #[must_use]
    pub(crate) fn error(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) -> Diagnostic {
        self.diagnostic(DiagnosticLevel::Error, message, notes, example)
    }

    #[must_use]
    pub(crate) fn warning_with_trace(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) -> Diagnostic {
        self.diagnostic_with_trace(DiagnosticLevel::Warning, message, notes, example, trace)
    }

    #[must_use]
    pub(crate) fn error_with_trace(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) -> Diagnostic {
        self.diagnostic_with_trace(DiagnosticLevel::Error, message, notes, example, trace)
    }

    pub(crate) fn add_diagnostic(&self, diagnostic: Diagnostic) {
        self.diagnostics.add(diagnostic);
    }

    pub(crate) fn add_diagnostic_with(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) {
        self.add_diagnostic_with_trace(level, message, notes, example, self.backtrace());
    }

    pub(crate) fn add_diagnostic_with_trace(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) {
        self.add_diagnostic(self.diagnostic_with_trace(level, message, notes, example, trace));
    }

    pub(crate) fn add_warning(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) {
        self.add_diagnostic_with(DiagnosticLevel::Warning, message, notes, example);
    }

    pub(crate) fn add_error(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
    ) {
        self.add_diagnostic_with(DiagnosticLevel::Error, message, notes, example);
    }

    pub(crate) fn add_warning_with_trace(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) {
        self.add_diagnostic_with_trace(DiagnosticLevel::Warning, message, notes, example, trace);
    }

    pub(crate) fn add_error_with_trace(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        example: impl ToString,
        trace: Backtrace,
    ) {
        self.add_diagnostic_with_trace(DiagnosticLevel::Error, message, notes, example, trace);
    }
}

impl Note {
    pub fn new(level: NoteLevel, span: impl Into<SpanList>, message: impl ToString) -> Self {
        Note {
            level,
            span: span.into(),
            message: message.to_string(),
            use_caller_if_available: false,
        }
    }

    pub fn primary(span: impl Into<SpanList>, message: impl ToString) -> Self {
        Note::new(NoteLevel::Primary, span, message)
    }

    pub fn secondary(span: impl Into<SpanList>, message: impl ToString) -> Self {
        Note::new(NoteLevel::Secondary, span, message)
    }

    pub fn use_caller_if_available(mut self) -> Self {
        self.use_caller_if_available = true;
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct Diagnostics {
    pub diagnostics: Shared<Vec<Diagnostic>>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add(&self, diagnostic: Diagnostic) {
        self.diagnostics.lock().push(diagnostic);
    }

    pub fn contains_errors(&self) -> bool {
        self.diagnostics
            .lock()
            .iter()
            .any(|d| matches!(d.level, DiagnosticLevel::Error))
    }

    pub fn highest_level(&self) -> Option<DiagnosticLevel> {
        self.diagnostics.lock().iter().map(|d| d.level).max()
    }
}

#[derive(Debug)]
pub struct FinalizedDiagnostics {
    pub source_map: SourceMap,
    pub diagnostics: Vec<Diagnostic>,
}

impl FinalizedDiagnostics {
    pub fn contains_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.level, DiagnosticLevel::Error))
    }

    pub fn highest_level(&self) -> Option<DiagnosticLevel> {
        self.diagnostics.iter().map(|d| d.level).max()
    }

    pub fn into_console_friendly(
        self,
        make_link: impl Fn(&str) -> String,
        show_expansion_history: bool,
        #[cfg(debug_assertions)] include_trace: bool,
    ) -> (
        codespan_reporting::files::SimpleFiles<FilePath, Arc<str>>,
        Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
    ) {
        use codespan_reporting::{
            diagnostic::{Diagnostic, Label, LabelStyle},
            files::SimpleFiles,
        };

        let diagnostics = self.diagnostics.into_iter().unique().collect::<Vec<_>>();

        let mut files = SimpleFiles::new();
        let mut console_diagnostics = Vec::new();

        let mut tracked_files = HashMap::<FilePath, usize>::new();
        for diagnostic in diagnostics {
            let mut make_note = |style: LabelStyle, span: Span, message: &str, use_caller: bool| {
                self.source_map.get(&span.path).map(|src| {
                    let file = match tracked_files.entry(span.path) {
                        Entry::Occupied(entry) => *entry.get(),
                        Entry::Vacant(entry) => {
                            let file = files.add(span.path, src.clone());
                            entry.insert(file);
                            file
                        }
                    };

                    let range = if use_caller {
                        span.caller_range().unwrap_or_else(|| span.primary_range())
                    } else {
                        span.primary_range()
                    };

                    Label::new(style, file, range.start.utf8..range.end.utf8).with_message(message)
                })
            };

            let primary_span = diagnostic
                .notes
                .first()
                .unwrap_or_else(|| {
                    panic!(
                        "diagnostic contains no notes, source: {:?}",
                        diagnostic.trace
                    )
                })
                .span;

            let (_, rest) = primary_span.split_iter();
            let rest: Box<dyn Iterator<Item = Span>> = if show_expansion_history {
                Box::new(rest)
            } else {
                Box::new(rest.last().into_iter())
            };

            let labels = diagnostic
                .notes
                .into_iter()
                .flat_map(|note| {
                    let (first, _) = note.span.split_iter();
                    make_note(
                        note.level.into(),
                        first,
                        &note.message,
                        note.use_caller_if_available,
                    )
                })
                .collect::<Vec<_>>()
                .into_iter()
                .chain(rest.flat_map(|span| {
                    make_note(
                        LabelStyle::Secondary,
                        span,
                        &format!(
                            "actual {} occurred here",
                            match diagnostic.level {
                                DiagnosticLevel::Error => "error",
                                DiagnosticLevel::Warning => "warning",
                            }
                        ),
                        false,
                    )
                }))
                .collect::<Vec<_>>();

            let mut notes = Vec::from_iter(
                diagnostic
                    .fix
                    .map(|fix| format!("{}: `{}`", fix.description, fix.replacement)),
            );

            if !diagnostic.example.is_empty() {
                notes.push(format!(
                    "for more information, see {}",
                    make_link(&diagnostic.example)
                ));
            }

            let diagnostic = Diagnostic::new(diagnostic.level.into())
                .with_message((|| {
                    #[cfg(debug_assertions)]
                    if include_trace {
                        if let Some(trace) = diagnostic.trace.into_inner() {
                            return diagnostic.message
                                + &format!("\nat span {primary_span:#?}\n{trace:#?}");
                        }
                    }

                    diagnostic.message
                })())
                .with_labels(labels)
                .with_notes(notes);

            console_diagnostics.push(diagnostic);
        }

        console_diagnostics.sort_by(|a, b| match (a.labels.first(), b.labels.first()) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => a.range.start.cmp(&b.range.start),
        });

        (files, console_diagnostics)
    }
}
