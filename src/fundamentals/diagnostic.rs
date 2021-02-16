use std::fmt::Display;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ProgramStack {
    pub items: Vec<ProgramStackItem>,
}

impl ProgramStack {
    pub fn new() -> ProgramStack {
        ProgramStack { items: Vec::new() }
    }

    pub fn add_item(&self, item: ProgramStackItem) -> ProgramStack {
        ProgramStack {
            items: {
                let mut items = self.items.clone();
                items.push(item);
                items
            },
        }
    }

    pub fn add(&self, label: &str) -> ProgramStack {
        let item = ProgramStackItem {
            label: String::from(label),
            location: None,
        };

        ProgramStack {
            items: {
                let mut items = self.items.clone();
                items.push(item);
                items
            },
        }
    }

    pub fn add_located(&self, label: &str, location: &SourceLocation) -> ProgramStack {
        let item = ProgramStackItem {
            label: String::from(label),
            location: Some(location.clone()),
        };

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
