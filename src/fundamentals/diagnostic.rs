use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct ProgramError {
    pub message: String,
}

impl ProgramError {
    pub fn new(message: &str) -> ProgramError {
        ProgramError {
            message: String::from(message),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramStack {
    pub items: Vec<ProgramStackItem>,
}

impl ProgramStack {
    pub fn new() -> ProgramStack {
        ProgramStack { items: Vec::new() }
    }

    pub fn add(&self, item: ProgramStackItem) -> ProgramStack {
        ProgramStack {
            items: {
                let mut items = self.items.clone();
                items.push(item);
                items
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramStackItem {
    pub label: String,
    pub location: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
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
