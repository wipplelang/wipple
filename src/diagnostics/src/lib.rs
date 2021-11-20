use codemap::CodeMap;
use interned_string::InternedString;
use serde::Serialize;
use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    fmt,
    ops::Range,
    sync::Arc,
};

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

#[derive(Serialize)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub notes: Vec<Note>,
}

#[derive(Serialize)]
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

#[derive(Serialize)]
pub struct Note {
    pub level: NoteLevel,
    pub span: Span,
    pub message: String,
}

#[derive(Serialize)]
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
        }
    }
}

impl Note {
    pub fn primary(span: Span, message: impl ToString) -> Self {
        Note {
            level: NoteLevel::Primary,
            span,
            message: message.to_string(),
        }
    }

    pub fn secondary(span: Span, message: impl ToString) -> Self {
        Note {
            level: NoteLevel::Secondary,
            span,
            message: message.to_string(),
        }
    }
}

#[derive(Default, Serialize)]
pub struct Diagnostics {
    pub files: HashMap<InternedString, Arc<str>>,
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_file(&mut self, path: InternedString, code: Arc<str>) {
        self.files.insert(path, code);
    }

    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn into_console_friendly(self) -> (CodeMap, Vec<codemap_diagnostic::Diagnostic>) {
        let mut codemap = CodeMap::new();
        let mut diagnostics = Vec::new();

        let files = &self.files;
        let mut tracked_files = HashMap::<_, Arc<codemap::File>>::new();
        for diagnostic in self.diagnostics {
            let diagnostic = codemap_diagnostic::Diagnostic {
                level: diagnostic.level.into(),
                message: diagnostic.message,
                code: None,
                spans: diagnostic
                    .notes
                    .into_iter()
                    .map(|note| {
                        let file = match tracked_files.entry(note.span.file) {
                            Entry::Occupied(entry) => entry.get().clone(),
                            Entry::Vacant(entry) => {
                                let file = codemap.add_file(
                                    note.span.file.to_string(),
                                    files
                                        .get(&note.span.file)
                                        .expect("Diagnostic references unknown file")
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

        (codemap, diagnostics)
    }
}
