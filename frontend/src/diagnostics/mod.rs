use crate::{
    helpers::{Backtrace, Shared},
    parse::Span,
    Compiler, FilePath, SourceMap,
};
use itertools::Itertools;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub notes: Vec<Note>,
    pub trace: Backtrace,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level && self.message == other.message && self.notes == other.notes
    }
}

impl Eq for Diagnostic {}

impl std::hash::Hash for Diagnostic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.level.hash(state);
        self.message.hash(state);
        self.notes.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Note {
    pub level: NoteLevel,
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
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

#[allow(unused)]
impl Compiler<'_> {
    pub(crate) fn diagnostic(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
    ) -> Diagnostic {
        self.diagnostic_with(level, message, notes, self.backtrace())
    }

    pub(crate) fn diagnostic_with(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message: message.to_string(),
            notes,
            trace,
        }
    }

    pub(crate) fn warning(&self, message: impl ToString, notes: Vec<Note>) -> Diagnostic {
        self.diagnostic(DiagnosticLevel::Warning, message, notes)
    }

    pub(crate) fn error(&self, message: impl ToString, notes: Vec<Note>) -> Diagnostic {
        self.diagnostic(DiagnosticLevel::Error, message, notes)
    }

    pub(crate) fn warning_with(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) -> Diagnostic {
        self.diagnostic_with(DiagnosticLevel::Warning, message, notes, trace)
    }

    pub(crate) fn error_with(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) -> Diagnostic {
        self.diagnostic_with(DiagnosticLevel::Error, message, notes, trace)
    }

    pub(crate) fn add_diagnostic(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
    ) {
        self.add_diagnostic_with(level, message, notes, self.backtrace());
    }

    pub(crate) fn add_diagnostic_with(
        &self,
        level: DiagnosticLevel,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) {
        self.diagnostics
            .add(self.diagnostic_with(level, message, notes, trace));
    }

    pub(crate) fn add_warning(&self, message: impl ToString, notes: Vec<Note>) {
        self.add_diagnostic(DiagnosticLevel::Warning, message, notes);
    }

    pub(crate) fn add_error(&self, message: impl ToString, notes: Vec<Note>) {
        self.add_diagnostic(DiagnosticLevel::Error, message, notes);
    }

    pub(crate) fn add_warning_with(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) {
        self.add_diagnostic_with(DiagnosticLevel::Warning, message, notes, trace);
    }

    pub(crate) fn add_error_with(
        &self,
        message: impl ToString,
        notes: Vec<Note>,
        trace: Backtrace,
    ) {
        self.add_diagnostic_with(DiagnosticLevel::Error, message, notes, trace);
    }
}

impl Note {
    pub fn new(level: NoteLevel, span: Span, message: impl ToString) -> Self {
        Note {
            level,
            span,
            message: message.to_string(),
        }
    }

    pub fn primary(span: Span, message: impl ToString) -> Self {
        Note::new(NoteLevel::Primary, span, message)
    }

    pub fn secondary(span: Span, message: impl ToString) -> Self {
        Note::new(NoteLevel::Secondary, span, message)
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

    #[cfg(feature = "serde_json")]
    pub fn to_json(self, w: impl std::io::Write) -> serde_json::Result<()> {
        serde_json::to_writer(w, &self.diagnostics)
    }

    pub fn into_console_friendly(
        self,
        #[cfg(debug_assertions)] include_trace: bool,
    ) -> (
        codespan_reporting::files::SimpleFiles<FilePath, Arc<str>>,
        Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
    ) {
        let diagnostics = self.diagnostics.into_iter().unique().collect::<Vec<_>>();

        let mut files = codespan_reporting::files::SimpleFiles::new();
        let mut console_diagnostics = Vec::new();

        let mut tracked_files = HashMap::<FilePath, usize>::new();
        for diagnostic in diagnostics {
            let labels = diagnostic
                .notes
                .into_iter()
                .filter_map(|note| {
                    let file = match tracked_files.entry(note.span.path) {
                        Entry::Occupied(entry) => *entry.get(),
                        Entry::Vacant(entry) => {
                            let file = files.add(
                                note.span.path,
                                self.source_map.get(&note.span.path)?.clone(),
                            );

                            entry.insert(file);

                            file
                        }
                    };

                    Some(
                        codespan_reporting::diagnostic::Label::new(
                            note.level.into(),
                            file,
                            note.span.start..note.span.end,
                        )
                        .with_message(note.message),
                    )
                })
                .collect();

            let diagnostic =
                codespan_reporting::diagnostic::Diagnostic::new(diagnostic.level.into())
                    .with_message((|| {
                        #[cfg(debug_assertions)]
                        if include_trace {
                            if let Some(trace) = diagnostic.trace.into_inner() {
                                return diagnostic.message + &format!("\n{:?}", trace);
                            }
                        }

                        diagnostic.message
                    })())
                    .with_labels(labels);

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
