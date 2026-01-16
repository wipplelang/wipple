use crate::database::{Db, Fact, NodeRef, Render};
use regex::Regex;
use serde::Serialize;
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    sync::LazyLock,
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Span {
    pub path: String,
    pub start: Location,
    pub end: Location,
    pub source: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

impl Fact for Span {}

impl Span {
    pub fn empty() -> Span {
        Span {
            path: String::new(),
            start: Location::empty(),
            end: Location::empty(),
            source: String::new(),
        }
    }

    pub fn join(left: Self, right: Self, source: &str) -> Self {
        Span {
            path: left.path.clone(),
            start: left.start.clone(),
            end: right.end.clone(),
            source: source[left.start.index..right.end.index.max(left.start.index)].to_string(),
        }
    }

    pub fn is_inside(&self, other: &Self) -> bool {
        self.path == other.path
            && self.start.index >= other.start.index
            && self.end.index <= other.end.index
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}-{}:{}",
            self.path, self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Render for Span {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "at {}", self)
    }
}

impl Location {
    pub fn empty() -> Location {
        Location {
            line: 1,
            column: 1,
            index: 0,
        }
    }
}

impl Db {
    pub fn have_equal_spans(&self, left: &NodeRef, right: &NodeRef) -> bool {
        self.span(left) == self.span(right)
    }
}

impl Span {
    pub fn as_node_source(&self) -> String {
        static BRACES_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r#"(?s)\{.*\}"#).unwrap());

        static ASSIGNMENT_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r#"(?s)::?.*"#).unwrap());

        static BOUNDS_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r#"(?s)\bwhere\b.*"#).unwrap());

        static LINES_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"(?s)\n.*"#).unwrap());

        let source = self.as_definition_source();

        // Collapse braces
        let source = BRACES_REGEX.replace_all(&source, "{⋯}");

        let source = if let Some(left_parentheses_index) = source.find('(')
            && let Some(annotate_index) = source.find("::")
            && annotate_index < left_parentheses_index
        {
            // Remove assignments and type annotations
            ASSIGNMENT_REGEX.replace_all(&source, "")
        } else {
            source
        };

        // Remove bounds
        let source = BOUNDS_REGEX.replace_all(&source, "");

        // Collapse multiple lines
        let source = LINES_REGEX.replace_all(&source, "⋯");

        source.trim().to_string()
    }

    pub fn as_definition_source(&self) -> String {
        static ATTRIBUTES_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r#"(\[.*\]\s*)*"#).unwrap());

        static COMMENTS_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"--.*\n"#).unwrap());

        // Strip attributes
        let source = ATTRIBUTES_REGEX.replace_all(&self.source, "");

        // Strip comments
        let source = COMMENTS_REGEX.replace_all(&source, "");

        // Remove parentheses
        let source = if source != "()" && source.starts_with("(") && source.ends_with(")") {
            let left_count = source.chars().take_while(|&c| c == '(').count();
            let right_count = source.chars().rev().take_while(|&c| c == ')').count();

            let trim_count = left_count.min(right_count);

            Cow::Borrowed(&source[trim_count..source.len() - trim_count])
        } else {
            source
        };

        // Remove assigned value...
        let source = if let Some(index) = source.find(":") {
            // ...but not type annotations or type/trait definitions
            let rest = &source[index + 1..];

            if !rest.starts_with(":") && !rest.contains(" type ") && !rest.contains(" trait ") {
                Cow::Borrowed(&source[..index])
            } else {
                source
            }
        } else {
            source
        };

        source.trim().to_string()
    }
}
