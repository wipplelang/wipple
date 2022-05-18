use crate::{parser::Span, FilePath};
use codemap::CodeMap;
use serde::Serialize;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

#[derive(Debug, Serialize)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub notes: Vec<Note>,

    #[cfg(debug_assertions)]
    pub trace: backtrace::Backtrace,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.level == other.level && self.message == other.message && self.notes == other.notes
    }
}

impl Eq for Diagnostic {}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Note,
    Warning,
    Error,
}

impl From<DiagnosticLevel> for codemap_diagnostic::Level {
    fn from(level: DiagnosticLevel) -> Self {
        match level {
            DiagnosticLevel::Note => codemap_diagnostic::Level::Note,
            DiagnosticLevel::Warning => codemap_diagnostic::Level::Warning,
            DiagnosticLevel::Error => codemap_diagnostic::Level::Error,
        }
    }
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Note {
    pub level: NoteLevel,
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum NoteLevel {
    Primary,
    Secondary,
}

impl From<NoteLevel> for codemap_diagnostic::SpanStyle {
    fn from(level: NoteLevel) -> Self {
        match level {
            NoteLevel::Primary => codemap_diagnostic::SpanStyle::Primary,
            NoteLevel::Secondary => codemap_diagnostic::SpanStyle::Secondary,
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
            trace: backtrace::Backtrace::new(),
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

#[derive(Debug, Default)]
pub struct Diagnostics {
    pub files: HashMap<FilePath, Arc<str>>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_file(&mut self, path: FilePath, code: Arc<str>) {
        self.files.insert(path, code);
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn contains_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.level, DiagnosticLevel::Error))
    }

    pub fn into_console_friendly(
        mut self,
        #[cfg(debug_assertions)] include_trace: bool,
    ) -> (
        CodeMap,
        HashMap<FilePath, Arc<codemap::File>>,
        Vec<codemap_diagnostic::Diagnostic>,
    ) {
        self.diagnostics.dedup();

        let mut codemap = CodeMap::new();
        let mut diagnostics = Vec::new();

        let files = &self.files;
        let mut tracked_files = HashMap::<FilePath, Arc<codemap::File>>::new();
        for diagnostic in self.diagnostics {
            let diagnostic = codemap_diagnostic::Diagnostic {
                level: diagnostic.level.into(),

                #[cfg(debug_assertions)]
                message: if include_trace {
                    diagnostic.message + &format!("\n{:?}", diagnostic.trace)
                } else {
                    diagnostic.message
                },

                #[cfg(not(debug_assertions))]
                message: diagnostic.message,

                code: None,

                spans: diagnostic
                    .notes
                    .into_iter()
                    .map(|note| {
                        let file = match tracked_files.entry(note.span.path) {
                            Entry::Occupied(entry) => entry.get().clone(),
                            Entry::Vacant(entry) => {
                                let file = codemap.add_file(
                                    note.span.path.to_string(),
                                    files
                                        .get(&note.span.path)
                                        .unwrap_or_else(|| {
                                            panic!(
                                                "diagnostic references unknown file: '{}'",
                                                note.span.path
                                            )
                                        })
                                        .to_string(),
                                );

                                entry.insert(file.clone());

                                file
                            }
                        };

                        codemap_diagnostic::SpanLabel {
                            span: file
                                .span
                                .subspan(note.span.start as u64, note.span.end as u64),
                            style: note.level.into(),
                            label: Some(note.message),
                        }
                    })
                    .collect(),
            };

            diagnostics.push(diagnostic);
        }

        diagnostics.sort_by(|a, b| match (a.spans.first(), b.spans.first()) {
            (None, None) => Ordering::Equal,
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(a), Some(b)) => a.span.low().cmp(&b.span.low()),
        });

        (codemap, tracked_files, diagnostics)
    }
}
