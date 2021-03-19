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

// TODO: Use dynamic key-based storage like Environment
#[derive(Clone)]
pub struct Stack {
    pub items: Vec<StackItem>,
    pub project_root: Option<PathBuf>,
    pub current_file: Option<PathBuf>,
    queued_location: Option<SourceLocation>,
    recording_enabled: bool,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            items: vec![],
            project_root: None,
            current_file: None,
            queued_location: None,
            recording_enabled: true,
        }
    }

    pub fn queue_location(&mut self, location: &SourceLocation) {
        self.queued_location = Some(location.clone());
    }

    pub fn disable_recording(&mut self) {
        self.recording_enabled = false;
    }

    pub fn add_item(&self, item: impl FnOnce() -> StackItem) -> Self {
        if !self.recording_enabled {
            return self.clone();
        }

        let mut item = item();
        if item.location.is_none() {
            item.location = self.queued_location.clone();
        }

        #[cfg(feature = "log_diagnostics")]
        println!("{}{}", "  ".repeat(self.items.len()), item.label);

        let mut stack = self.clone();
        stack.items.push(item);
        stack.queued_location = None;
        stack.recording_enabled = true;

        stack
    }

    pub fn add(&self, label: impl FnOnce() -> String) -> Self {
        self.add_item(|| StackItem {
            label: label(),
            location: None,
        })
    }

    pub fn add_location(
        &self,
        label: impl FnOnce() -> String,
        location: Option<SourceLocation>,
    ) -> Self {
        self.add_item(|| StackItem {
            label: label(),
            location,
        })
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Stack {
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
pub enum ReturnState {
    ReturnFromBlock,
    BreakOutOfLoop,
    Error(Error),
}

impl ReturnState {
    /// Call when all other return states have made it to the top level,
    /// converting them into errors.
    pub fn into_error(self, stack: &Stack) -> Error {
        use ReturnState::*;

        match self {
            ReturnFromBlock => crate::Error::new("'return' outside block", stack),
            BreakOutOfLoop => crate::Error::new("'break' outside loop", stack),
            Error(error) => error,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub stack: Stack,
}

impl Error {
    pub fn new(message: &str, stack: &Stack) -> Self {
        Error {
            message: String::from(message),
            stack: stack.clone(),
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
