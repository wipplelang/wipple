use std::path::PathBuf;

use crate::*;
use derivative::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    pub file: Option<PathBuf>,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn new(file: Option<PathBuf>, line: usize, column: usize) -> Self {
        SourceLocation { file, line, column }
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.file {
            Some(file) => f.write_str(&format!(
                "{}:{}:{}",
                file.to_string_lossy(),
                self.line,
                self.column
            )),
            None => f.write_str(&format!("{}:{}", self.line, self.column)),
        }
    }
}

#[derive(Derivative, TypeInfo, Debug, Clone, PartialEq, Eq, Hash)]
#[derivative(Default)]
pub struct DiagnosticsStack {
    pub items: Vec<EvaluationStackItem>,

    queued_location: Option<SourceLocation>,

    #[derivative(Default(value = "true"))]
    recording_enabled: bool,
}

impl DiagnosticsStack {
    pub fn queue_location(&mut self, location: Option<&SourceLocation>) {
        if location.is_some() {
            self.queued_location = location.cloned();
        }
    }

    pub fn disable_recording(&mut self) {
        self.recording_enabled = false;
    }

    pub fn add_item(&mut self, item: impl FnOnce() -> EvaluationStackItem) {
        if !self.recording_enabled {
            return;
        }

        let mut item = item();
        if item.location.is_none() {
            item.location = self.queued_location.clone();
        }

        #[cfg(feature = "log_diagnostics")]
        println!("{}{}", "  ".repeat(self.items.len()), item.label);

        self.items.push(item);
        self.queued_location = None;
        self.recording_enabled = true;
    }

    pub fn add(&mut self, label: impl FnOnce() -> String) {
        self.add_item(|| EvaluationStackItem {
            label: label(),
            location: None,
        })
    }

    pub fn add_location(
        &mut self,
        label: impl FnOnce() -> String,
        location: Option<SourceLocation>,
    ) {
        self.add_item(|| EvaluationStackItem {
            label: label(),
            location,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EvaluationStackItem {
    pub label: String,
    pub location: Option<SourceLocation>,
}

impl std::fmt::Display for EvaluationStackItem {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.location {
            Some(location) => f.write_str(&format!("{} ({})", self.label, location)),
            None => f.write_str(&self.label),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exit {
    Return(Value, Stack),
    Break(Value, Stack),
    Error(Error),
}

#[macro_export]
macro_rules! catch {
    ($exit:ident in $expr:expr) => {
        match $expr {
            Ok(value) | Err(Exit::$exit(value, _)) => Ok(value),
            Err(exit) => Err(exit),
        }
    };
}

#[macro_export]
macro_rules! catch_only {
    ($exit:ident in $expr:expr) => {
        match $expr {
            Ok(_) => Ok(None),
            Err(Exit::$exit(value, _)) => Ok(Some(value)),
            Err(exit) => Err(exit),
        }
    };
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub stack: DiagnosticsStack,
}

stack_key!(pub diagnostics: DiagnosticsStack);

impl Error {
    pub fn new(message: &str, stack: &Stack) -> Self {
        Error {
            message: message.to_string(),
            stack: stack.diagnostics().into_owned(),
        }
    }
}

pub fn error(message: &str, stack: &Stack) -> Exit {
    Exit::Error(Error::new(message, stack))
}

impl Exit {
    pub fn into_error(self) -> Error {
        match self {
            Exit::Return(_, stack) => Error::new("'return' outside block", &stack),
            Exit::Break(_, stack) => Error::new("'break' outside loop", &stack),
            Exit::Error(error) => error,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let stack = self
            .stack
            .items
            .iter()
            .rev()
            .map(|s| format!("    {}", s))
            .collect::<Vec<_>>()
            .join("\n");

        f.write_str(&format!("{}\n{}", self.message, stack))
    }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Exit>;
