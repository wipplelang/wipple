use crate::{parse::Span, FilePath, SourceMap};
use parking_lot::Mutex;
use serde::Serialize;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    mem,
    sync::Arc,
};

#[cfg(debug_assertions)]
lazy_static::lazy_static! {
    static ref BACKTRACE_ENABLED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);
}

#[cfg(debug_assertions)]
pub fn backtrace_enabled() -> bool {
    BACKTRACE_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

#[cfg(debug_assertions)]
pub fn set_backtrace_enabled(enabled: bool) {
    BACKTRACE_ENABLED.store(enabled, std::sync::atomic::Ordering::Relaxed);
}

#[derive(Debug, Clone, Serialize)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub notes: Vec<Note>,

    #[cfg(debug_assertions)]
    pub trace: Option<backtrace::Backtrace>,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level && self.message == other.message && self.notes == other.notes
    }
}

impl Eq for Diagnostic {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum DiagnosticLevel {
    Note,
    Warning,
    Error,
}

impl From<DiagnosticLevel> for codespan_reporting::diagnostic::Severity {
    fn from(level: DiagnosticLevel) -> Self {
        match level {
            DiagnosticLevel::Note => codespan_reporting::diagnostic::Severity::Note,
            DiagnosticLevel::Warning => codespan_reporting::diagnostic::Severity::Warning,
            DiagnosticLevel::Error => codespan_reporting::diagnostic::Severity::Error,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Note {
    pub level: NoteLevel,
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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

impl Diagnostic {
    pub fn new(level: DiagnosticLevel, message: impl ToString, notes: Vec<Note>) -> Self {
        Diagnostic {
            level,
            message: message.to_string(),
            notes,

            #[cfg(debug_assertions)]
            trace: backtrace_enabled().then(backtrace::Backtrace::new),
        }
    }

    pub fn note(message: impl ToString, notes: Vec<Note>) -> Self {
        Diagnostic::new(DiagnosticLevel::Note, message, notes)
    }

    pub fn warning(message: impl ToString, notes: Vec<Note>) -> Self {
        Diagnostic::new(DiagnosticLevel::Warning, message, notes)
    }

    pub fn error(message: impl ToString, notes: Vec<Note>) -> Self {
        Diagnostic::new(DiagnosticLevel::Error, message, notes)
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
    pub diagnostics: Arc<Mutex<Vec<Diagnostic>>>,
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

    #[cfg(feature = "serde_json")]
    pub fn to_json(self, w: impl std::io::Write) -> serde_json::Result<()> {
        serde_json::to_writer(w, &self.diagnostics)
    }

    pub fn into_console_friendly(
        &mut self,
        #[cfg(debug_assertions)] include_trace: bool,
    ) -> (
        codespan_reporting::files::SimpleFiles<FilePath, Arc<str>>,
        Vec<codespan_reporting::diagnostic::Diagnostic<usize>>,
    ) {
        let mut diagnostics = mem::take(&mut self.diagnostics);
        diagnostics.dedup();

        let mut files = codespan_reporting::files::SimpleFiles::new();
        let mut console_diagnostics = Vec::new();

        let mut tracked_files = HashMap::<FilePath, usize>::new();
        for diagnostic in diagnostics {
            let labels = diagnostic
                .notes
                .into_iter()
                .map(|note| {
                    let file = match tracked_files.entry(note.span.path) {
                        Entry::Occupied(entry) => *entry.get(),
                        Entry::Vacant(entry) => {
                            let file = files.add(
                                note.span.path,
                                self.source_map
                                    .get(&note.span.path)
                                    .unwrap_or_else(|| {
                                        panic!("file {:?} not loaded", note.span.path)
                                    })
                                    .clone(),
                            );

                            entry.insert(file);

                            file
                        }
                    };

                    codespan_reporting::diagnostic::Label::new(
                        note.level.into(),
                        file,
                        note.span.start..note.span.end,
                    )
                    .with_message(note.message)
                })
                .collect();

            let diagnostic =
                codespan_reporting::diagnostic::Diagnostic::new(diagnostic.level.into())
                    .with_message((|| {
                        #[cfg(debug_assertions)]
                        if include_trace {
                            if let Some(trace) = diagnostic.trace {
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
