//! Render compiler errors, types, and other items to strings.

use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::Range};
use wipple_util::WithInfo;

/// A rendered error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Error {
    /// The group the error belongs to.
    pub group: ErrorGroup,

    /// The error's primary label.
    pub primary_label: Label,

    /// The error's secondary labels.
    pub secondary_labels: Vec<Label>,

    /// An extended help message for the error.
    pub help: String,

    /// A fix for the error.
    pub fix: Option<Fix>,
}

/// A category of [`Error`]s.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ErrorGroup {
    /// The name of the error.
    pub name: String,

    /// An explanation of the error.
    pub explanation: String,

    /// The ID of an example showing how to fix the error.
    pub example: String,
}

/// A label in an [`Error`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Label {
    /// The path to the file containing the error.
    pub file: String,

    /// The location in the source code where the error occurred.
    pub span: Range<u32>,

    /// The error message.
    pub message: String,
}

/// A fix for an [`Error`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Fix {
    /// The action's message.
    pub message: String,

    /// Code to insert before the labeled code.
    pub before: Option<String>,

    /// The replacement for the labeled code.
    pub replacement: Option<String>,

    /// Code to insert after the labeled code.
    pub after: Option<String>,
}

impl Fix {
    fn before(message: impl ToString, before: impl ToString) -> Self {
        Self {
            message: message.to_string(),
            before: Some(before.to_string()),
            replacement: None,
            after: None,
        }
    }

    fn replace(message: impl ToString, replacement: impl ToString) -> Self {
        Self {
            message: message.to_string(),
            before: None,
            replacement: Some(replacement.to_string()),
            after: None,
        }
    }

    fn after(message: impl ToString, after: impl ToString) -> Self {
        Self {
            message: message.to_string(),
            before: None,
            replacement: None,
            after: Some(after.to_string()),
        }
    }
}

/// Render a compiler error to an [`Error`].
pub fn render_error(error: WithInfo<crate::Info, crate::Error>) -> Error {
    let info = error.info;

    let todo_error = || Error {
        group: ErrorGroup {
            name: String::from("TODO"),
            explanation: String::new(),
            example: String::new(),
        },
        primary_label: Label {
            file: String::new(),
            span: Range::default(),
            message: String::from("TODO"),
        },
        secondary_labels: Vec::new(),
        help: String::new(),
        fix: None,
    };

    match error.item {
        crate::Error::Read(error) => {
            let group = ErrorGroup {
                name: String::from("Syntax error"),
                explanation: String::from("Wipple couldn't understand your code because a symbol is missing or is in the wrong place."),
                example: String::from("syntax-error"),
            };

            match error.kind {
                wipple_parser::reader::ErrorKind::InvalidToken => Error {
                    group,
                    primary_label: Label {
                        file: info.parser_info.path,
                        span: error.span,
                        message: String::from("this symbol isn't recognized"),
                    },
                    secondary_labels: Vec::new(),
                    fix: Some(Fix::replace("remove the symbol", "")),
                    help: String::from("Wipple recognizes symbols like parentheses, math operators, numbers, and names containing `A` through `Z` and dashes (`-`). Make sure your code only uses these symbols."),
                },
                wipple_parser::reader::ErrorKind::Mismatch { expected, found } => {
                    let render_token = |token: &wipple_parser::reader::TokenKind<'_>,
                                        a: &str,
                                        an: &str| match token
                    {
                        wipple_parser::reader::TokenKind::LeftParenthesis => {
                            format!("{an}opening `(`")
                        }
                        wipple_parser::reader::TokenKind::RightParenthesis => {
                            format!("{a}closing `)`")
                        }
                        wipple_parser::reader::TokenKind::LineBreak => {
                            format!("{a}new line")
                        }
                        wipple_parser::reader::TokenKind::Comment(_) => {
                            format!("{a}comment")
                        }
                        wipple_parser::reader::TokenKind::Symbol(symbol) => {
                            format!("`{symbol}`")
                        }
                        wipple_parser::reader::TokenKind::Text(_) => {
                            format!("{a}piece of text")
                        }
                        wipple_parser::reader::TokenKind::Number(_) => {
                            format!("{a}number")
                        }
                        wipple_parser::reader::TokenKind::Asset(_) => {
                            format!("{a}color, image, or emoji")
                        }
                    };

                    Error {
                        group,
                        primary_label: Label {
                            file: info.parser_info.path,
                            span: error.span,
                            message: match (&expected, &found) {
                                (None, None) => unreachable!(),
                                (None, Some(found)) => {
                                    format!("extra {} here", render_token(found, "", ""))
                                }
                                (Some(expected), None) => {
                                    format!("missing {} here", render_token(expected, "a ", "an "))
                                }
                                (Some(expected), Some(found)) => format!(
                                    "expected {} here, but found {} instead",
                                    render_token(expected, "a ", "an "),
                                    render_token(found, "a ", "an ")
                                ),
                            },
                        },
                        secondary_labels: Vec::new(),
                        fix: match (&expected, &found) {
                            (None, None) => unreachable!(),
                            (None, Some(token)) => Some(Fix::replace(format!("remove the {}", render_token(token, "", "")), "")),
                            (Some(token), None) => {
                                let replacement = match token {
                                    wipple_parser::reader::TokenKind::RightParenthesis => ")",
                                    _ => unimplemented!(),
                                };

                                Some(Fix::replace(format!("add {}", render_token(token, "a ", "an ")), replacement))
                            },
                            (Some(expected), Some(found)) => {
                                let replacement = match expected {
                                    wipple_parser::reader::TokenKind::RightParenthesis => ")",
                                    _ => unimplemented!(),
                                };

                                Some(Fix::replace(format!("remove the {} and add {}", render_token(found, "", ""), render_token(expected, "a ", "an ")), replacement))
                            },
                        },
                        help: String::from("Parentheses are used to group code together — the opening `(` tells Wipple where to start and the closing `)` tells Wipple where to end. Make sure you have a closing `)` for every opening `(`."),
                    }
                }
                /*


#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, IntoDiagnostic)]
#[file_id(String)]
pub enum ReadError {
    #[message = "this symbol isn't allowed here"]
    InvalidToken,

    #[message = "expected {expected} here, but found {found} instead"]
    Mismatch { expected: String, found: String },

    #[message = "this operator can't be used multiple times in the same line of code"]
    MultipleNonAssociativeOperators,

    #[message = "missing an input before this operator"]
    MissingOperatorInputOnLeft,

    #[message = "missing an input after this operator"]
    MissingOperatorInputOnRight,
}

                 */
                wipple_parser::reader::ErrorKind::MultipleNonAssociativeOperators { operator, first_span } => Error {
                    group,
                    primary_label: Label {
                        file: info.parser_info.path.clone(),
                        span: error.span,
                        message: format!("the `{operator}` operator can't be used multiple times in the same line of code"),
                    },
                    secondary_labels: vec![Label {
                        file: info.parser_info.path,
                        span: first_span,
                        message: format!("first use of `{operator}` here"),
                    }],
                    help: format!("Operators like `+` prioritize the left or right side automatically. For example, `1 + 2 + 3` is the same as `(1 + 2) + 3`. However, `{operator}` doesn't prioritize either side, so you need to use parentheses."),
                    fix: None,
                },
                wipple_parser::reader::ErrorKind::MissingOperatorInputOnLeft(operator) => Error {
                    group,
                    primary_label: Label {
                        file: info.parser_info.path,
                        span: error.span,
                        message: format!("missing an input before the `{operator}` operator"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("The `{operator}` operator needs an input on the left side."),
                    fix: Some(Fix::before(format!("add an input to the left of the `{operator}`"), "_ ")),
                },
                wipple_parser::reader::ErrorKind::MissingOperatorInputOnRight(operator) => Error {
                    group,
                    primary_label: Label {
                        file: info.parser_info.path,
                        span: error.span,
                        message: format!("missing an input after the `{operator}` operator"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("The `{operator}` operator needs an input on the right side."),
                    fix: Some(Fix::after(format!("add an input to the right of the `{operator}`"), " _")),
                },
            }
        }
        crate::Error::Parse(_) => todo_error(),
        crate::Error::Syntax(_) => todo_error(),
        crate::Error::Lower(_) => todo_error(),
        crate::Error::Typecheck(_) => todo_error(),
    }
}

pub fn colorize_errors<'a>(
    errors: impl IntoIterator<Item = &'a Error>,
    source_code_for_file: impl Fn(&str) -> String,
) -> String {
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term,
    };

    let mut files = SimpleFiles::new();
    let mut file_ids = HashMap::new();

    let mut output = Vec::new();
    let mut writer = term::termcolor::Ansi::new(&mut output);

    let config = term::Config::default();

    for error in errors {
        for label in std::iter::once(&error.primary_label).chain(&error.secondary_labels) {
            use std::collections::hash_map::Entry;

            match file_ids.entry(&label.file) {
                Entry::Occupied(_) => continue,
                Entry::Vacant(entry) => {
                    let file_id = files.add(&label.file, source_code_for_file(&label.file));
                    entry.insert(file_id);
                }
            }
        }

        let diagnostic = Diagnostic::error() // TODO: Warnings
            .with_message(&error.group.name)
            .with_labels(
                std::iter::once(
                    Label::primary(
                        *file_ids.get(&error.primary_label.file).unwrap(),
                        (error.primary_label.span.start as usize)
                            ..(error.primary_label.span.end as usize),
                    )
                    .with_message(&error.primary_label.message),
                )
                .chain(error.secondary_labels.iter().map(|label| {
                    Label::secondary(
                        *file_ids.get(&label.file).unwrap(),
                        (label.span.start as usize)..(label.span.end as usize),
                    )
                    .with_message(&label.message)
                }))
                .collect::<Vec<_>>(),
            )
            .with_notes(
                std::iter::once(error.group.explanation.clone())
                    .chain(std::iter::once(error.help.clone()))
                    .chain(
                        error
                            .fix
                            .as_ref()
                            .map(|fix| format!("Help: {}", fix.message)),
                    )
                    .collect::<Vec<_>>(),
            );

        term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
    }

    String::from_utf8(output).unwrap()
}