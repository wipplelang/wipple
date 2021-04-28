use crate::*;
use std::{fmt, path::PathBuf};

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: Option<PathBuf>,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.file {
            Some(file) => write!(
                f,
                "{}:{}:{}",
                file.to_string_lossy(),
                self.line,
                self.column
            ),
            None => write!(f, "{}:{}", self.line, self.column),
        }
    }
}

#[derive(TypeInfo, Clone)]
pub struct EvaluationStack {
    pub items: Vec<StackItem>,
    queued_location: Option<SourceLocation>,
    recording_enabled: bool,
}

core_stack_key!(pub evaluation for EvaluationStack);

impl Default for EvaluationStack {
    fn default() -> Self {
        EvaluationStack {
            items: vec![],
            queued_location: None,
            recording_enabled: true,
        }
    }
}

impl EvaluationStack {
    pub fn queue_location(&mut self, location: &Option<SourceLocation>) {
        if location.is_some() {
            self.queued_location = location.clone();
        }
    }

    pub fn disable_recording(&mut self) {
        self.recording_enabled = false;
    }

    pub fn add_item(&mut self, item: impl FnOnce() -> StackItem) {
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
        self.add_item(|| StackItem {
            label: label(),
            location: None,
        })
    }

    pub fn add_location(
        &mut self,
        label: impl FnOnce() -> String,
        location: Option<SourceLocation>,
    ) {
        self.add_item(|| StackItem {
            label: label(),
            location,
        })
    }
}

impl fmt::Debug for EvaluationStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.items
                .iter()
                .rev()
                .map(|item| item.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone)]
pub struct StackItem {
    pub label: String,
    pub location: Option<SourceLocation>,
}

impl fmt::Display for StackItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.location {
            Some(location) => write!(f, "{} ({})", self.label, location),
            None => write!(f, "{}", self.label),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Return {
    Return(Value, Stack),
    Break(Value, Stack),
    Error(Error),
}

impl Return {
    pub fn r#return(value: Value, stack: &Stack) -> Self {
        Return::Return(value, stack.clone())
    }

    pub fn r#break(value: Value, stack: &Stack) -> Self {
        Return::Break(value, stack.clone())
    }

    pub fn error(message: &str, stack: &Stack) -> Self {
        Return::Error(Error::new(message, stack))
    }
}

impl Return {
    /// Call when all other returns have made it to the top level to convert
    /// them into errors.
    pub fn as_error(&self) -> Error {
        match &self {
            Return::Return(_, stack) => Error::new("'return' outside block", stack),
            Return::Break(_, stack) => Error::new("'break' outside loop", stack),
            Return::Error(error) => error.clone(),
        }
    }
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_error().fmt(f)
    }
}

impl std::error::Error for Return {}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub stack: EvaluationStack,
}

impl Error {
    pub fn new(message: &str, stack: &Stack) -> Self {
        Error {
            message: String::from(message),
            stack: stack.evaluation(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}\n{}",
            self.message,
            self.stack
                .items
                .iter()
                .rev()
                .map(|item| format!("    {}", item))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl std::error::Error for Error {}
