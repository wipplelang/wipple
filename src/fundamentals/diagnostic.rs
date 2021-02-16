use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramError {
    pub message: String,
    pub stack: ProgramStack,
}

impl ProgramError {
    pub fn new(message: &str, stack: &ProgramStack) -> ProgramError {
        ProgramError {
            message: String::from(message),
            stack: stack.clone(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramStack {
    pub items: Vec<ProgramStackItem>,
    queued_location: Option<SourceLocation>,
}

impl ProgramStack {
    pub fn new() -> ProgramStack {
        ProgramStack {
            items: Vec::new(),
            queued_location: None,
        }
    }

    pub fn queue_location(&mut self, location: &SourceLocation) {
        self.queued_location = Some(location.clone());
    }

    pub fn add_item(&self, mut item: ProgramStackItem) -> ProgramStack {
        if item.location.is_none() {
            if let Some(location) = &self.queued_location {
                item.location = Some(location.clone())
            }
        }

        ProgramStack {
            items: {
                let mut items = self.items.clone();
                items.push(item);
                items
            },
            queued_location: None,
        }
    }

    pub fn add(&self, label: &str) -> ProgramStack {
        self.add_item(ProgramStackItem {
            label: String::from(label),
            location: None,
        })
    }

    pub fn add_located(&self, label: &str, location: &SourceLocation) -> ProgramStack {
        let stack = ProgramStack {
            queued_location: None,
            ..self.clone()
        };

        stack.add_item(ProgramStackItem {
            label: String::from(label),
            location: Some(location.clone()),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramStackItem {
    pub label: String,
    pub location: Option<SourceLocation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    pub file: Option<String>,
    pub line: usize,
    pub column: usize,
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.file {
            Some(file) => write!(f, "{}:{}:{}", file, self.line, self.column),
            None => write!(f, "{}:{}", self.line, self.column),
        }
    }
}
