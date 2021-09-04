//! Log diagnostic information for a source file.

use codemap::{CodeMap, File};
use codemap_diagnostic::{Diagnostic, Level, SpanLabel, SpanStyle};
use lazy_static::lazy_static;
use std::{
    ops::Range,
    sync::{Arc, Mutex},
};

lazy_static! {
    static ref CODEMAP: Mutex<CodeMap> = Default::default();
}

/// Manages diagnostic information for a file.
pub struct Diagnostics {
    file: Arc<File>,
    diagnostics: Vec<codemap_diagnostic::Diagnostic>,
}

impl Diagnostics {
    /// Register a new file for use with diagnostics.
    pub fn new(file: String, code: String) -> Self {
        Diagnostics {
            file: CODEMAP.lock().unwrap().add_file(file, code),
            diagnostics: Vec::new(),
        }
    }

    /// Add a diagnostic item to the managed file.
    pub fn add(&mut self, location: Range<usize>, level: Level, msg: impl ToString) {
        let span = SpanLabel {
            span: self
                .file
                .span
                .subspan(location.start as u64, location.end as u64),
            label: None,
            style: SpanStyle::Primary,
        };

        let diagnostic = Diagnostic {
            level,
            message: msg.to_string(),
            code: None,
            spans: vec![span],
        };

        self.diagnostics.push(diagnostic);
    }
}
