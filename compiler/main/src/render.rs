//! Render compiler diagnostics, types, and other items to strings.

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::Range};
use wipple_util::WithInfo;

/// A rendered diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Diagnostic {
    /// Whether the diagnostic represents an error that must be fixed before the
    /// program can run.
    pub error: bool,

    /// The group the error belongs to.
    pub group: DiagnosticGroup,

    /// The error's primary label.
    pub primary_label: Label,

    /// The error's secondary labels.
    pub secondary_labels: Vec<Label>,

    /// An extended help message for the error.
    pub help: String,

    /// A fix for the error.
    pub fix: Option<Fix>,
}

/// A category of [`Diagnostic`]s.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiagnosticGroup {
    /// The name of the error.
    pub name: String,

    /// An explanation of the error.
    pub explanation: String,

    /// The ID of an example showing how to fix the error.
    pub example: String,
}

/// A label in a [`Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Label {
    /// The path to the file containing the error.
    pub path: String,

    /// The path to display to the user.
    pub visible_path: String,

    /// The location in the source code where the error occurred.
    pub span: Range<u32>,

    /// The error message.
    pub message: String,
}

/// A fix for a [`Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
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

/// Render a compiler diagnostic to an [`Diagnostic`].
pub fn render_diagnostic(
    error: WithInfo<crate::Info, crate::Diagnostic>,
    query: &crate::Query<'_>,
) -> Diagnostic {
    let info = error.info;

    match error.item {
        crate::Diagnostic::Read(error) => {
            let group = DiagnosticGroup {
                name: String::from("Syntax error"),
                explanation: String::from("Wipple couldn't understand your code because a symbol is missing or is in the wrong place."),
                example: String::from("syntax-error"),
            };

            match error.kind {
                wipple_parser::reader::ErrorKind::InvalidToken => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
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
                        wipple_parser::reader::TokenKind::LeftBrace => {
                            format!("{an}opening `{{`")
                        }
                        wipple_parser::reader::TokenKind::RightBrace => {
                            format!("{a}closing `}}`")
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
                    };

                    Diagnostic {
                        error: true,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path,
                            visible_path: info.parser_info.visible_path,
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
                                    wipple_parser::reader::TokenKind::RightBrace => "}",
                                    _ => unimplemented!(),
                                };

                                Some(Fix::replace(format!("add {}", render_token(token, "a ", "an ")), replacement))
                            },
                            (Some(expected), Some(found)) => {
                                let replacement = match expected {
                                    wipple_parser::reader::TokenKind::RightParenthesis => ")",
                                    wipple_parser::reader::TokenKind::RightBrace => "}",
                                    _ => unimplemented!(),
                                };

                                Some(Fix::replace(format!("remove the {} and add {}", render_token(found, "", ""), render_token(expected, "a ", "an ")), replacement))
                            },
                        },
                        help: String::from("Parentheses are used to group code together — the opening `(` tells Wipple where to start and the closing `)` tells Wipple where to end. Make sure you have a closing `)` for every opening `(`."),
                    }
                }
                wipple_parser::reader::ErrorKind::MultipleNonAssociativeOperators { operator, first_span } => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: error.span,
                        message: format!("the `{operator}` operator can't be used multiple times in the same line of code"),
                    },
                    secondary_labels: vec![Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: first_span,
                        message: format!("first use of `{operator}` here"),
                    }],
                    help: format!("Operators like `+` prioritize the left or right side automatically. For example, `1 + 2 + 3` is the same as `(1 + 2) + 3`. However, `{operator}` doesn't prioritize either side, so you need to use parentheses."),
                    fix: None,
                },
                wipple_parser::reader::ErrorKind::MissingOperatorInputOnLeft(operator) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: error.span,
                        message: format!("missing an input before the `{operator}` operator"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("The `{operator}` operator needs an input on the left side."),
                    fix: Some(Fix::before(format!("add an input to the left of the `{operator}`"), "_ ")),
                },
                wipple_parser::reader::ErrorKind::MissingOperatorInputOnRight(operator) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: error.span,
                        message: format!("missing an input after the `{operator}` operator"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("The `{operator}` operator needs an input on the right side."),
                    fix: Some(Fix::after(format!("add an input to the right of the `{operator}`"), " _")),
                },
            }
        }
        crate::Diagnostic::Parse(error) => {
            let group = DiagnosticGroup {
                name: String::from("Syntax error"),
                explanation: String::from("Wipple couldn't understand your code because it was expecting a different piece of code than the one provided."),
                example: String::from("syntax-error"),
            };

            let render_syntax_kind =
                |kind: &wipple_parser::syntax::SyntaxKind, a: &str, an: &str| match kind {
                    wipple_parser::syntax::SyntaxKind::Name => format!("{a}name"),
                    wipple_parser::syntax::SyntaxKind::Text => format!("{a}piece of text"),
                    wipple_parser::syntax::SyntaxKind::Block => format!("{a}block of code"),
                    wipple_parser::syntax::SyntaxKind::Instance => format!("{an}instance"),
                    wipple_parser::syntax::SyntaxKind::TypeParameter => {
                        format!("{a}type parameter")
                    }
                    wipple_parser::syntax::SyntaxKind::Trait => format!("{a}trait"),
                    wipple_parser::syntax::SyntaxKind::Pattern => format!("{a}pattern"),
                    wipple_parser::syntax::SyntaxKind::Expression => format!("{a}piece of code"),
                    wipple_parser::syntax::SyntaxKind::Type => format!("{a}type"),
                    wipple_parser::syntax::SyntaxKind::TypeMember => format!("{a}type member"),
                    wipple_parser::syntax::SyntaxKind::Arm => format!("{a}function"),
                    wipple_parser::syntax::SyntaxKind::TypeRepresentation => {
                        format!("{a}list of fields or variants")
                    }
                    wipple_parser::syntax::SyntaxKind::Nothing => String::from("no more code"),
                };

            let render_help = |kind: &wipple_parser::syntax::SyntaxKind| {
                match kind {
                    wipple_parser::syntax::SyntaxKind::Name => {
                        String::from("This code defines a new value and needs a name for that value. If you're trying to add parameters to a function, put them to the right of `:`, followed by an arrow.")
                    }
                    wipple_parser::syntax::SyntaxKind::Text => {
                        String::from("You might be forgetting quotes here.")
                    }
                    wipple_parser::syntax::SyntaxKind::Block => {
                        String::from("You might be forgetting parentheses here.")
                    }
                    wipple_parser::syntax::SyntaxKind::Instance => {
                        String::from("An instance looks like `(Show Number)` or `(Add Text Text Text)`.")
                    }
                    wipple_parser::syntax::SyntaxKind::TypeParameter => String::from("A type parameter is a single name like `A` or `Value`."),
                    wipple_parser::syntax::SyntaxKind::Trait => String::from("A trait is a single name like `Show` or `Add`."),
                    wipple_parser::syntax::SyntaxKind::Pattern => String::from("A pattern looks like `x` (to assign to a variable), `Some value` (to match a variant), `(number : 5)` (to match a structure), or `_` (to match anything)."),
                    wipple_parser::syntax::SyntaxKind::Expression => String::from("Try adding some more code here."),
                    wipple_parser::syntax::SyntaxKind::Type => String::from("Try adding another type here."),
                    wipple_parser::syntax::SyntaxKind::TypeMember => String::from("A type member looks like the field `name :: Text` (in a structure) or the variant `Blue` (in an enumeration)."),
                    wipple_parser::syntax::SyntaxKind::Arm => String::from("`when` accepts a list of functions to match its input. Make sure you are using the `->` arrow to define a function."),
                    wipple_parser::syntax::SyntaxKind::TypeRepresentation => String::from("`type` needs to know how to construct its values. Make sure you provide at least one field or variant."),
                    wipple_parser::syntax::SyntaxKind::Nothing => String::from("Try removing this code or moving it to a new line."),
                }
            };

            Diagnostic {
                error: true,
                group,
                primary_label: Label {
                    path: info.parser_info.path,
                    visible_path: info.parser_info.visible_path,
                    span: error.span,
                    message: format!(
                        "expected {} here",
                        render_syntax_kind(&error.expected, "a ", "an ")
                    ),
                },
                secondary_labels: Vec::new(),
                help: render_help(&error.expected),
                fix: None,
            }
        }
        crate::Diagnostic::Syntax(error) => {
            let group = DiagnosticGroup {
                name: String::from("Syntax error"),
                explanation: String::from(
                    "Wipple couldn't understand your code because some information is missing.",
                ),
                example: String::from("syntax-error"),
            };

            match error {
                wipple_syntax::Diagnostic::UnexpectedBound => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: String::from("unexpected bound here"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Bounds aren't allowed in `type` and `trait` definitions. Instead, move the bounds to the functions that use the types and traits."),
                    fix: None,
                },
                wipple_syntax::Diagnostic::ExpectedConstantValue(name) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: format!("`{name}`'s value must come immediately after its definition"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("Here, you defined `{name}`'s type using `::`, but constants must also be assigned a value using `:`. Try giving `{name}` a value on the line below this one."),
                    fix: Some(Fix::after(format!("give `{name}` a value"), format!("\n{name} : _"))),
                },
                wipple_syntax::Diagnostic::EmptyTypeRepresentation => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: String::from("type definition must contain at least one field or variant"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Try adding a field (`name :: Type`) or variant (`Name`) inside the parentheses."),
                    fix: Some(Fix::replace("add a new field between the parentheses", "(\n  field :: Text\n)")),
                },
                wipple_syntax::Diagnostic::ExpectedField => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: String::from("expected another field after the previous field in this structure definition"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("A type must contain all fields or all variants."),
                    fix: None,
                },
                wipple_syntax::Diagnostic::ExpectedVariant => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: String::from("expected another variant after the previous variant in this enumeration definition"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("A type must contain all fields or all variants."),
                    fix: None,
                },
                wipple_syntax::Diagnostic::InvalidTextLiteral(text_literal_error) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: (info.parser_info.span.start + text_literal_error.start)..(info.parser_info.span.start + text_literal_error.end),
                        message: text_literal_error.error,
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("This piece of text contains an invalid character or escape sequence."),
                    fix: Some(Fix::replace("remove the invalid piece of text", "")),
                },
                wipple_syntax::Diagnostic::InvalidPlaceholderText { expected, found } => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path,
                        visible_path: info.parser_info.visible_path,
                        span: info.parser_info.span,
                        message: format!("expected {} values after the text because it has {} placeholders", expected, found),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from(
                        if expected > found {
                            "Try adding some more inputs here."
                        } else {
                            "Try removing some of these inputs or adding more placeholders to the text."
                        }
                    ),
                    fix: None,
                },
            }
        }
        crate::Diagnostic::Lower(error) => {
            let group = DiagnosticGroup {
                name: String::from("Name error"),
                explanation: String::from(
                    "Wipple couldn't find the definition for a name in your code.",
                ),
                example: String::from("name-error"),
            };

            let names_related_to = |name: &str| {
                query.related_names(WithInfo {
                    info: info.clone(),
                    item: name,
                })
            };

            let render_unresolved = |kind: &str, name: &str| {
                let related_names = names_related_to(name);

                Diagnostic {
                    error: true,
                    group: group.clone(),
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: format!("can't find {kind}`{name}`"),
                    },
                    secondary_labels: related_names
                        .iter()
                        .map(|name| Label {
                            path: name.info.parser_info.path.clone(),
                            visible_path: name.info.parser_info.visible_path.clone(),
                            span: name.info.parser_info.span.clone(),
                            message: format!("`{}` is defined here, did you mean this?", name.item),
                        })
                        .collect(),
                    help: format!(
                        "Check your spelling or add a definition for `{name}` using `:`."
                    ),
                    fix: related_names.first().map(|name| {
                        Fix::replace(format!("did you mean `{}`?", name.item), name.item)
                    }),
                }
            };

            match error {
                wipple_lower::Diagnostic::UnresolvedName(name) => render_unresolved("", &name),
                wipple_lower::Diagnostic::UnresolvedType(name) => render_unresolved("type ", &name),
                wipple_lower::Diagnostic::UnresolvedTrait(name) => render_unresolved("trait ", &name),
                wipple_lower::Diagnostic::UnresolvedVariant(name) => {
                    render_unresolved("variant ", &name)
                }
                wipple_lower::Diagnostic::UnresolvedLanguageItem(name) => {
                    render_unresolved("language item ", &name)
                }
                wipple_lower::Diagnostic::AmbiguousName { name, candidates } => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: format!("`{name}` could refer to multiple definitions"),
                    },
                    secondary_labels: candidates
                        .iter()
                        .filter_map(|path| {
                            let info = query.info_at_path(path)?;

                            Some(Label {
                                path: info.parser_info.path.clone(),
                                visible_path: info.parser_info.visible_path.clone(),
                                span: info.parser_info.span,
                                message: format!(
                                    "`{}` could refer to this",
                                    path.last().unwrap().name()?,
                                ),
                            })
                        })
                        .collect(),
                    help: format!("Try renaming one of the definitions of `{name}` so it doesn't conflict with the others."),
                    fix: None,
                },
                wipple_lower::Diagnostic::AlreadyDefined(path) => {
                    let name = path.last().unwrap().name().unwrap();
                    let info = query.info_at_path(&path).unwrap();

                    Diagnostic {
                        error: true,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span.clone(),
                            message: format!("`{name}` is already defined"),
                        },
                        secondary_labels: vec![Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: format!("the other `{}` is defined here", name),
                        }],
                        help: format!("Try choosing a different name for `{name}` so it doesn't conflict with the existing definition."),
                        fix: None,
                    }
                },
                wipple_lower::Diagnostic::NestedLanguageDeclaration => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: String::from("`language` items must be defined at the top level"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Try moving this line outside of any code."),
                    fix: None,
                },
                wipple_lower::Diagnostic::InvalidComposition => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: String::from("missing an input to the `|` operator"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("The function composition operator `|` requires inputs on both sides."),
                    fix: None,
                },
                wipple_lower::Diagnostic::InvalidDeferredType => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: String::from("`defer` is not allowed here"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("`defer` may only appear on the input to a function."),
                    fix: None,
                },
            }
        }
        crate::Diagnostic::Typecheck(error) => {
            let group = DiagnosticGroup {
                name: String::from("Type error"),
                explanation: String::from("This code doesn't produce what's expected here. Double-check that you're providing the right inputs and using the correct units."),
                example: String::from("type-error"),
            };

            match error {
                wipple_typecheck::Diagnostic::RecursionLimit => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: String::from("recursion limit reached while checking this code"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("This code is too complex for Wipple to check. Try splitting the code across multiple functions or providing explicit type annotations using `::`."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::MissingLanguageItem(language_item) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: format!("language item `{language_item}` is required to check this code"),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("Try defining `{language_item}` using a `language` pattern."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::UnknownType(r#type) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span.clone(),
                        message: String::from("not enough information to determine the type of this code"),
                    },
                    secondary_labels: if let wipple_typecheck::Type::Unknown(_) = r#type {
                        Vec::new()
                    } else {
                        vec![Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: format!("this code produces a value of type `{}`, where the `_` placeholders are unknown", render_type(&r#type, true)),
                        }]
                    },
                    help: String::from("Try providing some more context so Wipple can check this code. One way to do this is by using `::` to explicitly annotate the type, but you can also try assigning this code to a variable and passing it to a function."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::UndeclaredTypeParameter(_) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: String::from(""),
                    },
                    secondary_labels: Vec::new(),
                    help: String::new(),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::Mismatch {
                    actual_roles,
                    actual,
                    expected_roles,
                    expected,
                } => {
                    let actual = render_type(&actual.item, true);
                    let expected = render_type(&expected.item, true);

                    // TODO: Conversions, etc.
                    let mut secondary_labels = Vec::new();

                    if let Some(role) = expected_roles.last() {
                        secondary_labels.push(Label {
                            path: role.info.parser_info.path.clone(),
                            visible_path: role.info.parser_info.visible_path.clone(),
                            span: role.info.parser_info.span.clone(),
                            message: {
                                let mut message = match role.item {
                                    wipple_typecheck::Role::Pattern => format!("expected because this pattern has type `{expected}`"),
                                    wipple_typecheck::Role::Annotation => String::from("expected because the type was annotated here"),
                                    wipple_typecheck::Role::Trait => format!("expected because this trait was defined to be a value of type `{expected}`"),
                                    wipple_typecheck::Role::Instance => format!("expected because this type in the instance is `{expected}`"),
                                    wipple_typecheck::Role::StructureField => format!("expected because this field in the structure has type `{expected}`"),
                                    wipple_typecheck::Role::VariantElement => format!("expected because this element of the variant has type `{expected}`"),
                                    wipple_typecheck::Role::FunctionInput => format!("expected because the input to the function has type `{expected}`"),
                                    wipple_typecheck::Role::FunctionOutput => format!("expected because the output of the function has type `{expected}`"),
                                    wipple_typecheck::Role::Bound => format!("expected because this type in the bound is `{expected}`"),
                                    wipple_typecheck::Role::DefaultType => format!("expected because the type defaults to `{expected}`"),
                                    wipple_typecheck::Role::Variable => format!("expected because this variable has type `{expected}`"),
                                    wipple_typecheck::Role::TypeParameter => format!("expected because this type parameter has type `{expected}`"),
                                    wipple_typecheck::Role::EmptyBlock => format!("expected because empty blocks have type `{expected}`"),
                                    wipple_typecheck::Role::WhenArm => format!("expected because this arm in the `when` expression has type `{expected}`"),
                                    wipple_typecheck::Role::CollectionElement => format!("expected because the previous elements in the collection have type `{expected}`"),
                                };

                                if !actual_roles.is_empty() {
                                    message.push_str("...");
                                }

                                message
                            },
                        });

                        if let Some(role) = actual_roles.last() {
                            secondary_labels.push(Label {
                                path: role.info.parser_info.path.clone(),
                                visible_path: role.info.parser_info.visible_path.clone(),
                                span: role.info.parser_info.span.clone(),
                                message: match role.item {
                                    wipple_typecheck::Role::Pattern => format!("...but this pattern actually has type `{actual}`"),
                                    wipple_typecheck::Role::Annotation => String::from("...but the actual type was annotated here"),
                                    wipple_typecheck::Role::Trait => format!("...but this trait was actually defined to be a value of type `{actual}`"),
                                    wipple_typecheck::Role::Instance => format!("...but this type in the instance is actually `{actual}`"),
                                    wipple_typecheck::Role::StructureField => format!("...but this field in the structure actually has type `{actual}`"),
                                    wipple_typecheck::Role::VariantElement => format!("...but this element of the variant actually has type `{actual}`"),
                                    wipple_typecheck::Role::FunctionInput => format!("...but the input to the function actually has type `{actual}`"),
                                    wipple_typecheck::Role::FunctionOutput => format!("...but the output of the function actually has type `{actual}`"),
                                    wipple_typecheck::Role::Bound => format!("...but this type in the bound is actually `{actual}`"),
                                    wipple_typecheck::Role::DefaultType => format!("...but the type actually defaults to `{actual}`"),
                                    wipple_typecheck::Role::Variable => format!("...but this variable actually has type `{actual}`"),
                                    wipple_typecheck::Role::TypeParameter => format!("...but this type parameter actually has type `{actual}`"),
                                    wipple_typecheck::Role::EmptyBlock => format!("...but empty blocks actually have type `{actual}`"),
                                    wipple_typecheck::Role::WhenArm => format!("...but this arm in the `when` expression actually has type `{actual}`"),
                                    wipple_typecheck::Role::CollectionElement => format!("...but the previous elements in the collection actually have type `{actual}`"),
                                }
                            });
                        }
                    }

                    Diagnostic {
                        error: true,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: format!("expected a `{expected}` here, but this code produces a `{actual}` instead"),
                        },
                        secondary_labels,
                        help: format!("Try adjusting the code so it produces a `{expected}` instead."),
                        fix: None,
                    }
                },
                wipple_typecheck::Diagnostic::DisallowedCoercion(r#type) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: format!("cannot automatically convert this code to a `{}`", render_type(&r#type.item, true)),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Try wrapping this code in a function that performs the conversion."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::UnresolvedInstance { instance, candidates, stack } => {
                    // TODO: Custom messages

                    let r#trait = instance.r#trait.last().unwrap().name().unwrap_or("_");

                    if candidates.len() > 1 {
                        Diagnostic {
                            error: true,
                            group,
                            primary_label: Label {
                                path: info.parser_info.path.clone(),
                                visible_path: info.parser_info.visible_path.clone(),
                                span: info.parser_info.span,
                                message: format!("multiple instances for `{trait}` could apply here"),
                            },
                            secondary_labels: candidates.into_iter().map(|candidate| {
                                Label {
                                    path: candidate.parser_info.path,
                                    visible_path: candidate.parser_info.visible_path,
                                    span: candidate.parser_info.span,
                                    message: String::from("this instance could apply"),
                                }
                            }).collect(),
                            help: String::from("Try providing some more context so only a single instance applies."),
                            fix: None,
                        }
                    } else {
                        Diagnostic {
                            error: true,
                            group,
                            primary_label: Label {
                                path: info.parser_info.path.clone(),
                                visible_path: info.parser_info.visible_path.clone(),
                                span: info.parser_info.span,
                                message: query.get_on_unimplemented_message(&instance.r#trait, &instance.parameters)
                                    .unwrap_or_else(|| format!("this code requires the instance `{}` to exist, but there is no such instance", render_instance(&instance))),
                            },
                            secondary_labels: if stack.len() > 1 {
                                stack
                                    .into_iter()
                                    .tuple_windows()
                                    .map(|(candidate, unsatisfied)| {
                                        Label {
                                            path: unsatisfied.info.parser_info.path,
                                            visible_path: unsatisfied.info.parser_info.visible_path,
                                            span: unsatisfied.info.parser_info.span,
                                            message: format!(
                                                "this instance could satisfy `{}`, but it requires `{}`",
                                                render_instance(&candidate.item),
                                                render_instance(&unsatisfied.item)
                                            ),
                                        }
                                    })
                                    .collect()
                            } else {
                                Vec::new()
                            },
                            help: format!("Try adjusting the code so it produces a type compatible with `{trait}`, or define a new instance for `{trait}` using `instance`."),
                            fix: None,
                        }
                    }
                },
                wipple_typecheck::Diagnostic::NotAStructure(r#type) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: format!("structure expression used here, but this code has type `{}`, which is not a structure type", render_type(&r#type.item, true)),
                    },
                    secondary_labels: Vec::new(),
                    help: format!("Try adjusting the code so it produces a value of type `{}`.", render_type(&r#type.item, true)),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::MissingFields(mut fields) => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: {
                            if fields.len() > 1 {
                                let last = fields.pop().unwrap();
                                format!("structure expression is missing the `{}` and `{}` fields", fields.join(", "), last)
                            } else {
                                format!("structure expression is missing the `{}` field", fields.first().unwrap())
                            }
                        },
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Try adding these fields using `:`."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::ExtraField => Diagnostic {
                    error: true,
                    group,
                    primary_label: Label {
                        path: info.parser_info.path.clone(),
                        visible_path: info.parser_info.visible_path.clone(),
                        span: info.parser_info.span,
                        message: String::from("extra field in structure expression"),
                    },
                    secondary_labels: Vec::new(),
                    help: String::from("Try removing this field or renaming it to one of the existing fields."),
                    fix: None,
                },
                wipple_typecheck::Diagnostic::OverlappingInstances { other, .. } => {
                    let other = query.info_at_path(&other).unwrap();

                    Diagnostic {
                        error: true,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: String::from("this instance is already defined"),
                        },
                        secondary_labels: vec![Label {
                            path: other.parser_info.path,
                            visible_path: other.parser_info.visible_path,
                            span: other.parser_info.span,
                            message: String::from("other definition of the instance here"),
                        }],
                        help: String::from("Try making this instance more specific or change the types involved."),
                        fix: None,
                    }
                },
                wipple_typecheck::Diagnostic::MissingPatterns(mut patterns) => {
                    Diagnostic {
                        error: true,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: match patterns.len() {
                                1 => format!("missing pattern for `{}`", render_pattern(query, patterns.pop().unwrap())),
                                _ => {
                                    let last = patterns.pop().unwrap();

                                    format!(
                                        "missing patterns for `{}` and `{}`",
                                        patterns.into_iter().map(|pattern| render_pattern(query, pattern)).join(", "),
                                        render_pattern(query, last),
                                    )
                                },
                            },
                        },
                        secondary_labels: Vec::new(),
                        help: String::from("The input could be one of these patterns, but your code doesn't handle them. Try adding some more cases or using `_` or a variable to match any possible value."),
                        fix: None,
                    }
                }
                wipple_typecheck::Diagnostic::ExtraPattern => {
                    Diagnostic {
                        error: false,
                        group,
                        primary_label: Label {
                            path: info.parser_info.path.clone(),
                            visible_path: info.parser_info.visible_path.clone(),
                            span: info.parser_info.span,
                            message: String::from("extra case in `when` expression"),
                        },
                        secondary_labels: Vec::new(),
                        help: String::from("This case is unreachable because a different pattern already handles the same pattern. Try removing this code."),
                        fix: None,
                    }
                },
                // FIXME: Rename 'Error' to 'Diagnostic'
            }
        }
    }
}

pub fn colorize_diagnostics<'a>(
    errors: impl IntoIterator<Item = &'a Diagnostic>,
    source_code_for_file: impl Fn(&str) -> String,
) -> String {
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label, Severity},
        files, term,
    };

    #[derive(Debug, Default)]
    struct FileMap(HashMap<String, files::SimpleFile<String, String>>);

    impl FileMap {
        fn add(&mut self, path: String, visible_path: String, source: String) {
            self.0
                .insert(path, files::SimpleFile::new(visible_path, source));
        }

        fn get(&self, path: &str) -> Result<&files::SimpleFile<String, String>, files::Error> {
            self.0.get(path).ok_or(files::Error::FileMissing)
        }
    }

    impl<'a> files::Files<'a> for FileMap {
        type FileId = &'a str;
        type Name = String;
        type Source = &'a str;

        fn name(&self, file_id: &'a str) -> Result<String, files::Error> {
            Ok(self.get(file_id)?.name().clone())
        }

        fn source(&self, file_id: &'a str) -> Result<&str, files::Error> {
            Ok(self.get(file_id)?.source().as_ref())
        }

        fn line_index(&self, file_id: &'a str, byte_index: usize) -> Result<usize, files::Error> {
            self.get(file_id)?.line_index((), byte_index)
        }

        fn line_range(
            &self,
            file_id: &'a str,
            line_index: usize,
        ) -> Result<Range<usize>, files::Error> {
            self.get(file_id)?.line_range((), line_index)
        }
    }

    let mut files = FileMap::default();

    let mut output = Vec::new();
    let mut writer = term::termcolor::Ansi::new(&mut output);

    let config = term::Config::default();

    for error in errors {
        for label in std::iter::once(&error.primary_label).chain(&error.secondary_labels) {
            if files.get(&label.path).is_err() {
                let source = source_code_for_file(&label.path);
                files.add(label.path.clone(), label.visible_path.clone(), source);
            }
        }

        let severity = if error.error {
            Severity::Error
        } else {
            Severity::Warning
        };

        let diagnostic = Diagnostic::new(severity)
            .with_message(&error.group.name)
            .with_labels(
                std::iter::once(
                    Label::primary(
                        error.primary_label.path.as_str(),
                        (error.primary_label.span.start as usize)
                            ..(error.primary_label.span.end as usize),
                    )
                    .with_message(&error.primary_label.message),
                )
                .chain(error.secondary_labels.iter().map(|label| {
                    Label::secondary(
                        error.primary_label.path.as_str(),
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

/// Render a type to a string.
pub fn render_type(r#type: &wipple_typecheck::Type<crate::Driver>, is_top_level: bool) -> String {
    fn render_type_inner(
        r#type: &wipple_typecheck::Type<crate::Driver>,
        is_top_level: bool,
        is_return: bool,
    ) -> String {
        match r#type {
            wipple_typecheck::Type::Unknown(_) => String::from("_"),
            wipple_typecheck::Type::Parameter(parameter) => {
                parameter.last().unwrap().name().unwrap_or("_").to_string()
            }
            wipple_typecheck::Type::Declared { path, parameters } => {
                let name = path.last().unwrap().name().unwrap_or("_");

                let rendered = if parameters.is_empty() {
                    name.to_string()
                } else {
                    format!(
                        "{} {}",
                        name,
                        parameters
                            .iter()
                            .map(|parameter| render_type_inner(&parameter.item, false, false))
                            .join(" ")
                    )
                };

                if is_top_level || parameters.is_empty() {
                    rendered
                } else {
                    format!("({})", rendered)
                }
            }
            wipple_typecheck::Type::Function { input, output } => {
                let input = render_type_inner(&input.item, is_top_level, false);
                let output = render_type_inner(&output.item, is_top_level, true);

                let rendered = format!("{} -> {}", input, output);

                if is_top_level && is_return {
                    rendered
                } else {
                    format!("({})", rendered)
                }
            }
            wipple_typecheck::Type::Tuple(elements) => {
                let rendered = match elements.len() {
                    0 => String::from("()"),
                    1 => format!(
                        "{} ;",
                        render_type_inner(&elements.first().unwrap().item, is_top_level, is_return)
                    ),
                    _ => format!(
                        "({})",
                        elements
                            .iter()
                            .map(|element| render_type_inner(&element.item, false, false))
                            .join(" ; ")
                    ),
                };

                if is_top_level || elements.is_empty() {
                    rendered
                } else {
                    format!("({})", rendered)
                }
            }
            wipple_typecheck::Type::Deferred(r#type) => {
                let rendered = render_type_inner(&r#type.item, false, false);

                if is_top_level {
                    format!("defer {rendered}")
                } else {
                    format!("(defer {})", rendered)
                }
            }
        }
    }

    render_type_inner(r#type, is_top_level, true)
}

/// Render an instance to a string.
pub fn render_instance(instance: &wipple_typecheck::Instance<crate::Driver>) -> String {
    let r#trait = instance.r#trait.last().unwrap().name().unwrap_or("_");

    let parameters = instance
        .parameters
        .iter()
        .map(|r#type| render_type(&r#type.item, false))
        .join(" ");

    format!("{} {}", r#trait, parameters)
}

/// The format to use when rendering a type function using
/// [`render_type_function`].
#[derive(Debug, Clone, Copy)]
pub enum TypeFunctionFormat {
    /// Render the type function in the format `A where (Show A) => A -> Text`.
    Arrow,

    /// Render the type function in the format `` `A -> Text` for any type `A` ``.
    Description,
}

/// Render a type function to a string.
pub fn render_type_function(
    format: TypeFunctionFormat,
    parameters: &[wipple_typecheck::Type<crate::Driver>],
    bounds: &[wipple_typecheck::Instance<crate::Driver>],
    r#type: &wipple_typecheck::Type<crate::Driver>,
) -> String {
    let bounds = bounds
        .iter()
        .map(|bound| format!("({})", render_instance(bound)))
        .join(" ");

    let bounds = if bounds.is_empty() {
        String::new()
    } else {
        format!(" where {}", bounds)
    };

    let r#type = render_type(r#type, true);

    if parameters.is_empty() && bounds.is_empty() {
        r#type
    } else {
        match format {
            TypeFunctionFormat::Arrow => {
                let parameters = parameters
                    .iter()
                    .map(|r#type| render_type(r#type, false))
                    .join(" ");
                format!("{parameters}{bounds} => {type}")
            }
            TypeFunctionFormat::Description => match parameters.len() {
                0 => unreachable!(),
                1 => {
                    let parameter = render_type(parameters.first().unwrap(), false);
                    format!("`{type}` for any type `{parameter}`")
                }
                _ => {
                    let mut parameters = parameters
                        .iter()
                        .map(|parameter| format!("`{}`", render_type(parameter, false)))
                        .collect::<Vec<_>>();

                    let last = parameters.pop().unwrap();

                    format!(
                        "`{}` for any types {} and {}",
                        r#type,
                        parameters.join(", "),
                        last
                    )
                }
            },
        }
    }
}

/// Render a pattern to a string.
pub fn render_pattern(
    _query: &crate::Query<'_>,
    pattern: wipple_typecheck::exhaustiveness::Pattern<crate::Driver>,
) -> String {
    fn render_pattern_inner(
        pattern: wipple_typecheck::exhaustiveness::Pattern<crate::Driver>,
        is_top_level: bool,
    ) -> String {
        match pattern {
            wipple_typecheck::exhaustiveness::Pattern::Constructor(constructor, mut patterns) => {
                match constructor {
                    wipple_typecheck::exhaustiveness::Constructor::Variant(path) => {
                        let name = path.last().unwrap().name().unwrap_or("_").to_string();

                        let has_patterns = !patterns.is_empty();

                        let rendered = if has_patterns {
                            let patterns = patterns
                                .into_iter()
                                .map(|pattern| render_pattern_inner(pattern, false))
                                .join(" ");

                            format!("{} {}", name, patterns)
                        } else {
                            name
                        };

                        if !is_top_level && !has_patterns {
                            format!("({})", rendered)
                        } else {
                            rendered
                        }
                    }
                    wipple_typecheck::exhaustiveness::Constructor::Tuple => {
                        let has_patterns = !patterns.is_empty();

                        let rendered = match patterns.len() {
                            1 => {
                                let pattern = patterns.pop().unwrap();
                                format!("{} ;", render_pattern_inner(pattern, false))
                            }
                            _ => {
                                let patterns = patterns
                                    .into_iter()
                                    .map(|pattern| render_pattern_inner(pattern, false))
                                    .join(" ; ");

                                format!("({})", patterns)
                            }
                        };

                        if !is_top_level && !has_patterns {
                            rendered
                        } else {
                            format!("({})", rendered)
                        }
                    }
                    wipple_typecheck::exhaustiveness::Constructor::Structure => {
                        // TODO: Render actual fields
                        String::from("(...)")
                    }
                    wipple_typecheck::exhaustiveness::Constructor::Unbounded => String::from("_"),
                }
            }
            wipple_typecheck::exhaustiveness::Pattern::Binding => String::from("_"),
            wipple_typecheck::exhaustiveness::Pattern::Or(_) => unreachable!("`or` is flattened"),
        }
    }

    render_pattern_inner(pattern, true)
}
