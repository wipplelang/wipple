//! Tokenize a source file and parse operators.

use itertools::Itertools;
use logos::Logos;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, collections::VecDeque, iter::Peekable, mem, ops::Range};

/// A token in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Token<'src> {
    /// The location of the token as a byte offset from the beginning of the
    /// source file.
    pub span: Range<u32>,

    /// The kind of token.
    pub kind: TokenKind<'src>,
}

/// The kind of [`Token`].
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Logos, Serialize, Deserialize)]
#[logos(skip r#"[\t ]+"#)]
#[serde(rename_all = "camelCase")]
pub enum TokenKind<'src> {
    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[regex(r#"\n"#)]
    LineBreak,

    #[regex(r#"\[[^\]\\]*(?s:\\.[^\]\\]*)*\]"#, |lex| Cow::Borrowed(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Comment(Cow<'src, str>),

    #[regex(r#"[~`!@#$%^&*\-+=\\\|:;<,>.?/]+|[A-Za-z0-9\-_]+[!?']?"#, |lex| Cow::Borrowed(lex.slice()))]
    Symbol(Cow<'src, str>),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*"|'[^'\\]*(?s:\\.[^'\\]*)*'"#, |lex| Cow::Borrowed(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Text(Cow<'src, str>),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| Cow::Borrowed(lex.slice()), priority = 2)]
    Number(Cow<'src, str>),

    #[regex(r#"`[^`]*`"#, |lex| Cow::Borrowed(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Asset(Cow<'src, str>),
}

#[allow(missing_docs)]
pub mod tokenize {
    use super::*;

    /// The result of [`tokenize`].
    #[derive(Debug, Serialize)]
    #[serde(rename_all = "camelCase", bound(serialize = "", deserialize = ""))]
    pub struct Result<'src> {
        /// The sequence of tokens.
        pub tokens: Vec<Token<'src>>,

        /// Any errors encountered while parsing the source code.
        pub errors: Vec<Error>,
    }
}

/// Tokenize a source file.
pub fn tokenize(s: &str) -> tokenize::Result<'_> {
    let (tokens, errors) = logos::Lexer::new(s)
        .spanned()
        .map(|(result, span)| {
            let span = (span.start as u32)..(span.end as u32);

            match result {
                Ok(kind) => Ok(Token { span, kind }),
                Err(()) => Err(Error::new(span, ErrorKind::InvalidToken)),
            }
        })
        .partition_result();

    tokenize::Result { tokens, errors }
}

/// A node in a syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Node<'src> {
    /// An empty node (eg. if the program contains no tokens or the node was a
    /// comment prior to stripping comments with [`ReadOptions::strip_comments`]).
    Empty,

    /// A terminal token (eg. a symbol or number literal).
    Token(Token<'src>),

    /// A list of nodes.
    List(Range<u32>, Vec<Node<'src>>),

    /// A block node.
    Block(Range<u32>, Vec<(Vec<Documentation>, Node<'src>)>),

    /// A use of a binary operator.
    BinaryOperator(Range<u32>, Token<'src>, BinaryOperatorInput<'src>),

    /// A use of a non-associative binary operator.
    NonAssociativeBinaryOperator(Range<u32>, Token<'src>, Box<Node<'src>>, Box<Node<'src>>),

    /// A use of a variadic operator.
    VariadicOperator(Range<u32>, Token<'src>, VariadicOperatorInput<'src>),
}

/// The parsed contents of a comment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum Documentation {
    /// A plain comment.
    Comment(String),

    /// A comment of the form `[name : value]`.
    Attribute {
        /// The name of the attribute.
        name: String,

        /// The value of the attribute.
        value: DocumentationAttributeValue,
    },
}

/// The value of a [`Documentation`] attribute.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum DocumentationAttributeValue {
    /// A text value wrapped in quotes.
    Text(String),

    /// A parsed expression, or [`Err`] if the expression was malformed.
    Code(std::result::Result<Node<'static>, String>),
}

/// The input to a [`Node::BinaryOperator`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BinaryOperatorInput<'src> {
    /// The operator has no inputs (eg. `(+)`).
    Unapplied,

    /// The operator has one input on its left (eg. `(1 +)`).
    PartiallyAppliedLeft(Box<Node<'src>>),

    /// The operator has one input on its right (eg. `(+ 1)`).
    PartiallyAppliedRight(Box<Node<'src>>),

    /// The operator has two inputs (eg. `(1 + 1)`).
    Applied(Box<Node<'src>>, Box<Node<'src>>),
}

/// The input to a [`Node::VariadicOperator`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum VariadicOperatorInput<'src> {
    /// The operator has no inputs (eg. `(,)`).
    Unapplied,

    /// The operator has multiple inputs (eg. `(1 , 2)` or `(1 ,)`).
    Applied(Vec<Node<'src>>),
}

impl<'src> Node<'src> {
    pub(crate) fn span(&self) -> Option<&Range<u32>> {
        match self {
            Node::Empty => None,
            Node::Token(token) => Some(&token.span),
            Node::List(span, ..)
            | Node::Block(span, ..)
            | Node::BinaryOperator(span, ..)
            | Node::NonAssociativeBinaryOperator(span, ..)
            | Node::VariadicOperator(span, ..) => Some(span),
        }
    }

    pub(crate) fn span_mut(&mut self) -> Option<&mut Range<u32>> {
        match self {
            Node::Empty => None,
            Node::Token(token) => Some(&mut token.span),
            Node::List(span, ..)
            | Node::Block(span, ..)
            | Node::BinaryOperator(span, ..)
            | Node::NonAssociativeBinaryOperator(span, ..)
            | Node::VariadicOperator(span, ..) => Some(span),
        }
    }

    fn as_list_mut(&mut self) -> Option<&mut Vec<Node<'src>>> {
        match self {
            Node::List(_, elements) => Some(elements),
            _ => None,
        }
    }
}

/// An error occurring during [`tokenize`] or [`read`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Error {
    /// The location in the source code where the error occurred.
    pub span: Range<u32>,

    /// The kind of error.
    pub kind: ErrorKind,
}

impl Error {
    pub(crate) fn new(span: Range<u32>, kind: ErrorKind) -> Self {
        Error { span, kind }
    }
}

/// The kind of [`Error`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ErrorKind {
    /// The tokenizer encountered an invalid token.
    InvalidToken,

    /// The tokenizer expected one token, but a different one was found.
    Mismatch {
        /// The expected token, or `None` if the end of the file was expected.
        expected: Option<TokenKind<'static>>,

        /// The provided token, or `None` if the end of the file was provided.
        found: Option<TokenKind<'static>>,
    },

    /// A non-associative operator was used multiple times in the same list.
    MultipleNonAssociativeOperators {
        /// The operator used.
        operator: String,

        /// The first use of the operator.
        first_span: Range<u32>,
    },

    /// The operator expected an input on the left side.
    MissingOperatorInputOnLeft(String),

    /// The operator expected an input on the right side.
    MissingOperatorInputOnRight(String),
}

/// Options for [`read`].
#[derive(Debug, Clone, Copy, Default)]
pub struct ReadOptions {
    /// Whether to remove comments from the syntax tree.
    pub strip_comments: bool,
}

#[allow(missing_docs)]
pub mod read {
    use super::*;

    /// The result of [`read`].
    #[derive(Debug, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Result<'src> {
        /// The top-level syntax tree.
        pub top_level: Node<'src>,

        /// Any errors encountered while parsing the sequence of tokens.
        pub errors: Vec<Error>,
    }
}

/// Convert a sequence of tokens into a syntax tree.
pub fn read(tokens: Vec<Token<'_>>, options: ReadOptions) -> read::Result<'_> {
    let mut errors = Vec::new();

    let first_span = tokens.first().map_or(0..0, |token| token.span.clone());
    let mut last_span = first_span.clone();

    let statements = read_block(
        &mut tokens.into_iter().peekable(),
        true,
        &mut last_span,
        &mut errors,
    );

    let block = Node::Block(first_span.start..last_span.end, statements);
    let mut top_level = read_operators(block, &mut errors);

    if options.strip_comments {
        top_level = top_level.strip_comments(&mut errors).unwrap_or(Node::Empty);
    }

    read::Result { top_level, errors }
}

impl<'src> Node<'src> {
    /// Remove all comments from this node.
    fn strip_comments(self, errors: &mut Vec<Error>) -> Option<Self> {
        match self {
            Node::Empty => Some(Node::Empty),
            Node::Token(token) => {
                if let TokenKind::Comment(_) = token.kind {
                    None
                } else {
                    Some(Node::Token(token))
                }
            }
            Node::List(span, elements) => Some(Node::List(
                span,
                elements
                    .into_iter()
                    .filter_map(|element| element.strip_comments(errors))
                    .collect(),
            )),
            Node::Block(span, statements) => Some(Node::Block(
                span,
                statements
                    .into_iter()
                    .filter_map(|(documentation, statement)| {
                        Some((documentation, statement.strip_comments(errors)?))
                    })
                    .collect(),
            )),
            Node::BinaryOperator(span, token, input) => Some(Node::BinaryOperator(
                span,
                token,
                input.strip_comments(errors),
            )),
            Node::NonAssociativeBinaryOperator(span, token, left, right) => {
                let operator_name = match &token.kind {
                    TokenKind::Symbol(symbol) => symbol,
                    _ => unreachable!(),
                };

                let left = match left.strip_comments(errors) {
                    Some(input) => input,
                    None => {
                        errors.push(Error::new(
                            span,
                            ErrorKind::MissingOperatorInputOnLeft(operator_name.to_string()),
                        ));

                        return None;
                    }
                };

                let right = match right.strip_comments(errors) {
                    Some(input) => input,
                    None => {
                        errors.push(Error::new(
                            span,
                            ErrorKind::MissingOperatorInputOnLeft(operator_name.to_string()),
                        ));

                        return None;
                    }
                };

                Some(Node::NonAssociativeBinaryOperator(
                    span,
                    token,
                    Box::new(left),
                    Box::new(right),
                ))
            }
            Node::VariadicOperator(span, token, input) => {
                let operator_name = match &token.kind {
                    TokenKind::Symbol(symbol) => symbol,
                    _ => unreachable!(),
                };

                let input = input.strip_comments(operator_name, &span, errors)?;
                Some(Node::VariadicOperator(span, token, input))
            }
        }
    }
}

impl<'src> BinaryOperatorInput<'src> {
    fn strip_comments(self, errors: &mut Vec<Error>) -> Self {
        match self {
            BinaryOperatorInput::Unapplied => BinaryOperatorInput::Unapplied,
            BinaryOperatorInput::PartiallyAppliedLeft(input) => {
                match input.strip_comments(errors) {
                    None => BinaryOperatorInput::Unapplied,
                    Some(input) => BinaryOperatorInput::PartiallyAppliedLeft(Box::new(input)),
                }
            }
            BinaryOperatorInput::PartiallyAppliedRight(input) => {
                match input.strip_comments(errors) {
                    None => BinaryOperatorInput::Unapplied,
                    Some(input) => BinaryOperatorInput::PartiallyAppliedRight(Box::new(input)),
                }
            }
            BinaryOperatorInput::Applied(left, right) => {
                match (left.strip_comments(errors), right.strip_comments(errors)) {
                    (None, None) => BinaryOperatorInput::Unapplied,
                    (None, Some(input)) => {
                        BinaryOperatorInput::PartiallyAppliedRight(Box::new(input))
                    }
                    (Some(input), None) => {
                        BinaryOperatorInput::PartiallyAppliedLeft(Box::new(input))
                    }
                    (Some(left), Some(right)) => {
                        BinaryOperatorInput::Applied(Box::new(left), Box::new(right))
                    }
                }
            }
        }
    }
}

impl<'src> VariadicOperatorInput<'src> {
    fn strip_comments(
        self,
        operator_name: &str,
        span: &Range<u32>,
        errors: &mut Vec<Error>,
    ) -> Option<Self> {
        match self {
            VariadicOperatorInput::Unapplied => Some(VariadicOperatorInput::Unapplied),
            VariadicOperatorInput::Applied(inputs) => {
                let inputs = inputs
                    .into_iter()
                    .filter_map(|mut input| {
                        let error = input
                            .span_mut()
                            .map(|span| {
                                Error::new(
                                    span.clone(),
                                    ErrorKind::MissingOperatorInputOnLeft(
                                        operator_name.to_string(),
                                    ),
                                )
                            })
                            .unwrap_or_else(|| {
                                Error::new(
                                    span.clone(),
                                    ErrorKind::MissingOperatorInputOnRight(
                                        operator_name.to_string(),
                                    ),
                                )
                            });

                        let input = input.strip_comments(errors);

                        if input.is_none() {
                            errors.push(error);
                        }

                        input
                    })
                    .collect::<Vec<_>>();

                if inputs.is_empty() {
                    None
                } else {
                    Some(VariadicOperatorInput::Applied(inputs))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Associativity {
    None,
    Left,
    Right,
    Variadic,
}

const PRECEDENCES: &[(Associativity, &[&str])] = &[
    (Associativity::None, &["of"]),
    (Associativity::Left, &["as"]),
    (Associativity::Right, &["^"]),
    (Associativity::Left, &["*", "/", "%"]),
    (Associativity::Left, &["+", "-"]),
    (Associativity::Left, &["<", "<=", ">", ">=", "=", "/="]),
    (Associativity::None, &["is"]),
    (Associativity::Left, &["and"]),
    (Associativity::Left, &["or"]),
    (Associativity::Left, &["."]),
    (Associativity::None, &["where"]),
    (Associativity::Variadic, &[";"]),
    (Associativity::Variadic, &[","]),
    (Associativity::Right, &["->"]),
    (Associativity::None, &["=>"]),
    (Associativity::None, &[":", "::"]),
];

fn is_operator(symbol: &str) -> bool {
    PRECEDENCES
        .iter()
        .flat_map(|&(_, operators)| operators)
        .any(|&operator| operator == symbol)
}

fn read_list<'src>(
    tokens: &mut Peekable<impl Iterator<Item = Token<'src>>>,
    last_span: &mut Range<u32>,
    errors: &mut Vec<Error>,
) -> Vec<Node<'src>> {
    let mut contents = Vec::new();
    loop {
        match tokens.next() {
            Some(next) => {
                *last_span = next.span.clone();

                match next.kind {
                    TokenKind::LeftParenthesis => {
                        if let Some(Token {
                            kind: TokenKind::LineBreak,
                            ..
                        }) = tokens.peek()
                        {
                            let statements = read_block(tokens, false, last_span, errors);
                            contents.push(Node::Block(last_span.clone(), statements));
                        } else {
                            let nodes = read_list(tokens, last_span, errors);
                            contents.push(Node::List(last_span.clone(), nodes));
                        }
                    }
                    TokenKind::RightParenthesis => return contents,
                    _ => contents.push(Node::Token(next)),
                }
            }
            None => {
                errors.push(Error::new(
                    last_span.clone(),
                    ErrorKind::Mismatch {
                        expected: Some(TokenKind::RightParenthesis),
                        found: None,
                    },
                ));

                return contents;
            }
        }
    }
}

fn read_block<'src>(
    tokens: &mut Peekable<impl Iterator<Item = Token<'src>>>,
    top_level: bool,
    last_span: &mut Range<u32>,
    errors: &mut Vec<Error>,
) -> Vec<(Vec<Documentation>, Node<'src>)> {
    let contents = (|| {
        let mut contents = vec![(Vec::new(), Node::List(last_span.clone(), Vec::new()))];

        macro_rules! consume_documentation {
            () => {
                std::iter::from_fn(|| match &tokens.peek()?.kind {
                    TokenKind::Comment(comment) => {
                        let documentation = read_documentation(comment);
                        tokens.next();

                        while let Some(Token {
                            kind: TokenKind::LineBreak,
                            ..
                        }) = tokens.peek()
                        {
                            tokens.next();
                        }

                        Some(documentation)
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
            };
        }

        contents
            .last_mut()
            .unwrap()
            .0
            .extend(consume_documentation!());

        loop {
            match tokens.next() {
                Some(next) => {
                    *last_span = next.span.clone();

                    contents
                        .last_mut()
                        .unwrap()
                        .0
                        .extend(consume_documentation!());

                    *last_span = next.span.clone();

                    match &next.kind {
                        TokenKind::LeftParenthesis => {
                            if let Some(Token {
                                kind: TokenKind::LineBreak,
                                ..
                            }) = tokens.peek()
                            {
                                let statements = read_block(tokens, false, last_span, errors);
                                contents
                                    .last_mut()
                                    .unwrap()
                                    .1
                                    .as_list_mut()
                                    .unwrap()
                                    .push(Node::Block(last_span.clone(), statements));
                            } else {
                                let nodes = read_list(tokens, last_span, errors);
                                contents
                                    .last_mut()
                                    .unwrap()
                                    .1
                                    .as_list_mut()
                                    .unwrap()
                                    .push(Node::List(last_span.clone(), nodes));
                            }
                        }
                        TokenKind::RightParenthesis => {
                            if top_level {
                                errors.push(Error::new(
                                    last_span.clone(),
                                    ErrorKind::Mismatch {
                                        expected: None,
                                        found: Some(TokenKind::RightParenthesis),
                                    },
                                ));
                            } else {
                                return contents;
                            }
                        }
                        // Allow splitting statements containing operators over
                        // multiple lines
                        TokenKind::Symbol(symbol) if is_operator(symbol) => {
                            contents
                                .last_mut()
                                .unwrap()
                                .1
                                .as_list_mut()
                                .unwrap()
                                .push(Node::Token(next));

                            while let Some(next) = tokens.peek() {
                                if let TokenKind::LineBreak = next.kind {
                                    tokens.next();
                                    continue;
                                } else {
                                    break;
                                }
                            }
                        }
                        // Same as above, but for the case where the operator is at
                        // the beginning of the line
                        TokenKind::LineBreak => {
                            while let Some(next) = tokens.peek().cloned() {
                                if let TokenKind::LineBreak = next.kind {
                                    tokens.next();
                                    continue;
                                } else {
                                    let documentation = consume_documentation!();

                                    if let TokenKind::Symbol(symbol) = &next.kind {
                                        if is_operator(symbol) {
                                            let next = tokens.next().unwrap();
                                            *last_span = next.span.clone();

                                            contents
                                                .last_mut()
                                                .unwrap()
                                                .1
                                                .as_list_mut()
                                                .unwrap()
                                                .push(Node::Token(next));

                                            break;
                                        }
                                    }

                                    contents.push((
                                        documentation,
                                        Node::List(last_span.clone(), Vec::new()),
                                    ));

                                    break;
                                }
                            }
                        }
                        _ => {
                            contents
                                .last_mut()
                                .unwrap()
                                .1
                                .as_list_mut()
                                .unwrap()
                                .push(Node::Token(next));
                        }
                    }
                }
                None => {
                    if !top_level {
                        errors.push(Error::new(
                            last_span.clone(),
                            ErrorKind::Mismatch {
                                expected: Some(TokenKind::RightParenthesis),
                                found: None,
                            },
                        ));
                    }

                    return contents;
                }
            }
        }
    })();

    // Remove empty statements
    contents
        .into_iter()
        .filter(|(_, node)| !matches!(node, Node::List(_, elements) if elements.is_empty()))
        .collect()
}

fn read_documentation(comment: &str) -> Documentation {
    if let Some((name, value)) = comment.split_once(" : ") {
        if value.starts_with('"') && value.ends_with('"') {
            return Documentation::Attribute {
                name: name.to_string(),
                value: DocumentationAttributeValue::Text(value[1..(value.len() - 1)].to_string()),
            };
        } else {
            let tokenize_result = tokenize(value);

            if tokenize_result.errors.is_empty() {
                let read_result = read(tokenize_result.tokens, ReadOptions::default());

                if read_result.errors.is_empty() {
                    return Documentation::Attribute {
                        name: name.to_string(),
                        value: DocumentationAttributeValue::Code(Ok(read_result
                            .top_level
                            .into_owned())),
                    };
                }
            }

            return Documentation::Attribute {
                name: name.to_string(),
                value: DocumentationAttributeValue::Code(Err(value.to_string())),
            };
        }
    }

    Documentation::Comment(comment.to_string())
}

fn read_operators<'src>(node: Node<'src>, errors: &mut Vec<Error>) -> Node<'src> {
    match node {
        Node::Empty => Node::Empty,
        Node::Token(token) => Node::Token(token),
        Node::List(span, mut elements) => {
            // Remove redundant parentheses
            loop {
                if elements.len() == 1 {
                    let node = elements.first_mut().unwrap();

                    if let Node::List(_, inner) = node {
                        if inner.is_empty() {
                            break;
                        } else {
                            elements = mem::take(inner);
                            continue;
                        }
                    }
                }

                break;
            }

            let mut operators = elements
                .iter()
                .enumerate()
                .filter_map(|(index, node)| {
                    let token = match node {
                        Node::Token(token) => token,
                        _ => return None,
                    };

                    match &token.kind {
                        TokenKind::Symbol(symbol) => PRECEDENCES
                            .iter()
                            .find_position(|(_, operators)| operators.contains(&symbol.as_ref()))
                            .map(|(precedence, (associativity, _))| {
                                (index, token.clone(), precedence, associativity)
                            }),
                        _ => None,
                    }
                })
                .max_set_by_key(|&(_, _, precedence, _)| precedence)
                .into_iter()
                .map(|(index, token, _, associativity)| (index, token, associativity))
                .collect::<Vec<_>>();

            let inputs = |mut side: Vec<Node<'src>>, errors: &mut Vec<Error>| {
                read_operators(
                    Node::List(
                        side.first_mut().unwrap().span_mut().unwrap().start
                            ..side.last_mut().unwrap().span_mut().unwrap().end,
                        side,
                    ),
                    errors,
                )
            };

            let input = |mut elements: Vec<_>, index: usize, errors: &mut Vec<Error>| {
                let right = elements
                    .split_off(index)
                    .into_iter()
                    .skip(1)
                    .collect::<Vec<_>>();

                let left = elements;

                match (left.is_empty(), right.is_empty()) {
                    (true, true) => BinaryOperatorInput::Unapplied,
                    (true, false) => {
                        BinaryOperatorInput::PartiallyAppliedRight(Box::new(inputs(right, errors)))
                    }
                    (false, true) => {
                        BinaryOperatorInput::PartiallyAppliedLeft(Box::new(inputs(left, errors)))
                    }
                    (false, false) => BinaryOperatorInput::Applied(
                        Box::new(inputs(left, errors)),
                        Box::new(inputs(right, errors)),
                    ),
                }
            };

            let variadic_input = |operators: Vec<(usize, Token<'src>, &Associativity)>,
                                  errors: &mut Vec<Error>| {
                let (_, first_operator_token, _) = operators.first().unwrap().clone();
                let mut operators = VecDeque::from(operators);

                let mut groups = vec![(None, Vec::new())];
                {
                    for (index, element) in elements.iter().enumerate() {
                        if let Some((operator_index, token)) = operators
                            .front()
                            .map(|(index, token, _)| (*index, token.clone()))
                        {
                            if index == operator_index {
                                operators.pop_front();
                                groups.push((Some((index, token.clone())), Vec::new()));
                                continue;
                            }
                        }

                        groups.last_mut().unwrap().1.push(element);
                    }
                }

                // Allow trailing operators
                if groups.last().unwrap().1.is_empty() {
                    groups.pop().unwrap();
                }

                let inputs = groups
                    .into_iter()
                    .map(Some)
                    .chain(std::iter::once(None))
                    .tuple_windows()
                    .filter_map(|(current, next)| {
                        let (operator, elements) = current.unwrap();

                        if elements.is_empty() {
                            if let Some((next_operator, _)) = next {
                                let (_, token) = next_operator.unwrap();

                                let operator_name = match token.kind {
                                    TokenKind::Symbol(symbol) => symbol.to_string(),
                                    _ => unreachable!(),
                                };

                                errors.push(Error::new(
                                    token.span,
                                    ErrorKind::MissingOperatorInputOnLeft(operator_name),
                                ));

                                None
                            } else {
                                // Allow a single trailing operator
                                let (_, operator) = operator.unwrap();
                                Some(Node::List(operator.span, Vec::new()))
                            }
                        } else {
                            Some(inputs(elements.into_iter().cloned().collect(), errors))
                        }
                    })
                    .collect::<Vec<_>>();

                (first_operator_token, VariadicOperatorInput::Applied(inputs))
            };

            match operators.len() {
                0 => {
                    if elements.len() == 1 {
                        read_operators(elements.pop().unwrap(), errors)
                    } else {
                        Node::List(
                            span,
                            elements
                                .into_iter()
                                .map(|element| read_operators(element, errors))
                                .collect(),
                        )
                    }
                }
                1 => {
                    let (index, token, associativity) = operators.last().unwrap();

                    let operator_name = match &token.kind {
                        TokenKind::Symbol(symbol) => symbol,
                        _ => unreachable!(),
                    };

                    match associativity {
                        Associativity::None => {
                            let (left, right) = match input(elements, *index, errors) {
                                BinaryOperatorInput::Unapplied
                                | BinaryOperatorInput::PartiallyAppliedRight(_) => {
                                    errors.push(Error::new(
                                        span,
                                        ErrorKind::MissingOperatorInputOnLeft(
                                            operator_name.to_string(),
                                        ),
                                    ));

                                    return Node::Empty;
                                }
                                BinaryOperatorInput::PartiallyAppliedLeft(_) => {
                                    errors.push(Error::new(
                                        span,
                                        ErrorKind::MissingOperatorInputOnRight(
                                            operator_name.to_string(),
                                        ),
                                    ));

                                    return Node::Empty;
                                }
                                BinaryOperatorInput::Applied(left, right) => (left, right),
                            };

                            Node::NonAssociativeBinaryOperator(span, token.clone(), left, right)
                        }
                        Associativity::Left | Associativity::Right => Node::BinaryOperator(
                            span,
                            token.clone(),
                            input(elements, *index, errors),
                        ),
                        Associativity::Variadic => {
                            let right = &elements[(index + 1)..];
                            let left = &elements[..*index];

                            let (token, input) = match (left.is_empty(), right.is_empty()) {
                                (true, true) => (token.clone(), VariadicOperatorInput::Unapplied),
                                (true, false) => {
                                    errors.push(Error::new(
                                        token.span.clone(),
                                        ErrorKind::MissingOperatorInputOnLeft(
                                            operator_name.to_string(),
                                        ),
                                    ));

                                    return Node::Empty;
                                }
                                (false, _) => {
                                    let token = token.clone();
                                    let (_, input) = variadic_input(operators, errors);
                                    (token, input)
                                }
                            };

                            Node::VariadicOperator(span, token, input)
                        }
                    }
                }
                _ => {
                    let (_, first_token, associativity) = operators.first().unwrap();

                    match associativity {
                        Associativity::None => {
                            for token in operators.iter().skip(1).map(|(_, token, _)| token.clone())
                            {
                                errors.push(Error::new(
                                    token.span.clone(),
                                    ErrorKind::MultipleNonAssociativeOperators {
                                        operator: match token.kind {
                                            TokenKind::Symbol(symbol) => symbol.to_string(),
                                            _ => unreachable!(),
                                        },
                                        first_span: first_token.span.clone(),
                                    },
                                ));
                            }

                            Node::Empty
                        }
                        Associativity::Left => {
                            let (index, token, _) = operators.pop().unwrap();
                            Node::BinaryOperator(span, token, input(elements, index, errors))
                        }
                        Associativity::Right => {
                            let (index, token, _) = operators.into_iter().next().unwrap();
                            Node::BinaryOperator(span, token, input(elements, index, errors))
                        }
                        Associativity::Variadic => {
                            let (token, input) = variadic_input(operators, errors);
                            Node::VariadicOperator(span, token, input)
                        }
                    }
                }
            }
        }
        Node::Block(span, statements) => Node::Block(
            span,
            statements
                .into_iter()
                .map(|(documentation, statement)| {
                    (documentation, read_operators(statement, errors))
                })
                .collect(),
        ),
        Node::BinaryOperator(..)
        | Node::NonAssociativeBinaryOperator(..)
        | Node::VariadicOperator(..) => {
            unreachable!()
        }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenKind::LeftParenthesis => write!(f, "`(`"),
            TokenKind::RightParenthesis => write!(f, "`)`"),
            TokenKind::LineBreak => write!(f, "line break"),
            TokenKind::Comment(s) => write!(f, "[{s}]"),
            TokenKind::Symbol(s) => write!(f, "{s}"),
            TokenKind::Text(s) => write!(f, "{s:?}"),
            TokenKind::Number(s) => write!(f, "{s}"),
            TokenKind::Asset(s) => write!(f, "`{s}`"),
        }
    }
}

#[derive(Debug)]
enum SExp {
    Symbol(String),
    String(String),
    List(Vec<SExp>),
}

impl SExp {
    fn symbol(s: impl ToString) -> Self {
        SExp::Symbol(s.to_string())
    }

    fn string(s: impl ToString) -> Self {
        SExp::String(s.to_string())
    }

    fn list(xs: impl IntoIterator<Item = SExp>) -> Self {
        SExp::List(Vec::from_iter(xs))
    }
}

impl SExp {
    /// Return a pretty printed format of self.
    pub fn to_doc(&self) -> pretty::RcDoc<'_, ()> {
        match self {
            SExp::Symbol(s) => pretty::RcDoc::as_string(s),
            SExp::String(s) => pretty::RcDoc::as_string(format!("{:?}", s)),
            SExp::List(xs) => pretty::RcDoc::text("(")
                .append(
                    pretty::RcDoc::intersperse(xs.iter().map(|x| x.to_doc()), pretty::Doc::line())
                        .nest(1)
                        .group(),
                )
                .append(pretty::RcDoc::text(")")),
        }
    }
}

impl std::fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_lexpr_value().to_doc().render_fmt(1, f)
    }
}

impl Token<'_> {
    fn to_lexpr_value(&self) -> SExp {
        match &self.kind {
            TokenKind::LeftParenthesis => SExp::string("("),
            TokenKind::RightParenthesis => SExp::string(")"),
            TokenKind::LineBreak => SExp::symbol("line-break"),
            TokenKind::Comment(_) => SExp::symbol("comment"),
            TokenKind::Symbol(name) => SExp::symbol(name),
            TokenKind::Text(text) => SExp::string(text),
            TokenKind::Number(s) => SExp::symbol(s),
            TokenKind::Asset(_) => SExp::symbol("asset"),
        }
    }
}

impl Node<'_> {
    fn to_lexpr_value(&self) -> SExp {
        match self {
            Node::Empty => SExp::list([SExp::symbol("empty")]),
            Node::Token(token) => SExp::list([SExp::symbol("token"), token.to_lexpr_value()]),
            Node::List(_, elements) => SExp::list(
                std::iter::once(SExp::symbol("list"))
                    .chain(elements.iter().map(Node::to_lexpr_value))
                    .collect::<Vec<_>>(),
            ),
            Node::Block(_, statements) => SExp::list(
                std::iter::once(SExp::symbol("block"))
                    .chain(statements.iter().map(|(_, contents)| {
                        SExp::list([SExp::symbol("statement"), contents.to_lexpr_value()])
                    }))
                    .collect::<Vec<_>>(),
            ),
            Node::BinaryOperator(_, operator, input) => SExp::list(
                std::iter::once(SExp::symbol("binary-operator"))
                    .chain(std::iter::once(SExp::list([
                        SExp::symbol("token"),
                        operator.to_lexpr_value(),
                    ])))
                    .chain(match input {
                        BinaryOperatorInput::Unapplied => Vec::new(),
                        BinaryOperatorInput::PartiallyAppliedLeft(input) => {
                            vec![SExp::list([SExp::symbol("left"), input.to_lexpr_value()])]
                        }
                        BinaryOperatorInput::PartiallyAppliedRight(input) => {
                            vec![SExp::list([SExp::symbol("right"), input.to_lexpr_value()])]
                        }
                        BinaryOperatorInput::Applied(left, right) => {
                            vec![left.to_lexpr_value(), right.to_lexpr_value()]
                        }
                    })
                    .collect::<Vec<_>>(),
            ),

            Node::NonAssociativeBinaryOperator(_, operator, left, right) => SExp::list([
                SExp::symbol("non-associative-binary-operator"),
                SExp::list([SExp::symbol("token"), operator.to_lexpr_value()]),
                SExp::list([SExp::symbol("left"), left.to_lexpr_value()]),
                SExp::list([SExp::symbol("right"), right.to_lexpr_value()]),
            ]),
            Node::VariadicOperator(_, operator, input) => SExp::list(
                std::iter::once(SExp::symbol("variadic-operator"))
                    .chain(std::iter::once(SExp::list([
                        SExp::symbol("token"),
                        operator.to_lexpr_value(),
                    ])))
                    .chain(match input {
                        VariadicOperatorInput::Unapplied => Vec::new(),
                        VariadicOperatorInput::Applied(contents) => {
                            contents.iter().map(Node::to_lexpr_value).collect()
                        }
                    })
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

impl<'src> Token<'src> {
    /// Produce a token that owns its contents.
    pub fn into_owned(self) -> Token<'static> {
        Token {
            span: self.span,
            kind: match self.kind {
                TokenKind::LeftParenthesis => TokenKind::LeftParenthesis,
                TokenKind::RightParenthesis => TokenKind::RightParenthesis,
                TokenKind::LineBreak => TokenKind::LineBreak,
                TokenKind::Comment(comment) => TokenKind::Comment(comment.into_owned().into()),
                TokenKind::Symbol(symbol) => TokenKind::Symbol(symbol.into_owned().into()),
                TokenKind::Text(text) => TokenKind::Text(text.into_owned().into()),
                TokenKind::Number(number) => TokenKind::Number(number.into_owned().into()),
                TokenKind::Asset(asset) => TokenKind::Asset(asset.into_owned().into()),
            },
        }
    }
}

impl<'src> Node<'src> {
    /// Produce a node that owns its contents.
    pub fn into_owned(self) -> Node<'static> {
        match self {
            Node::Empty => Node::Empty,
            Node::Token(token) => Node::Token(token.into_owned()),
            Node::List(span, elements) => Node::List(
                span,
                elements.into_iter().map(|node| node.into_owned()).collect(),
            ),
            Node::Block(span, statements) => Node::Block(
                span,
                statements
                    .into_iter()
                    .map(|(documentation, statement)| (documentation, statement.into_owned()))
                    .collect(),
            ),
            Node::BinaryOperator(span, operator, input) => {
                Node::BinaryOperator(span, operator.into_owned(), input.into_owned())
            }
            Node::NonAssociativeBinaryOperator(span, operator, left, right) => {
                Node::NonAssociativeBinaryOperator(
                    span,
                    operator.into_owned(),
                    Box::new(left.into_owned()),
                    Box::new(right.into_owned()),
                )
            }
            Node::VariadicOperator(span, operator, input) => {
                Node::VariadicOperator(span, operator.into_owned(), input.into_owned())
            }
        }
    }
}

impl<'src> BinaryOperatorInput<'src> {
    /// Produce a binary operator input that owns its contents.
    pub fn into_owned(self) -> BinaryOperatorInput<'static> {
        match self {
            BinaryOperatorInput::Unapplied => BinaryOperatorInput::Unapplied,
            BinaryOperatorInput::PartiallyAppliedLeft(input) => {
                BinaryOperatorInput::PartiallyAppliedLeft(Box::new(input.into_owned()))
            }
            BinaryOperatorInput::PartiallyAppliedRight(input) => {
                BinaryOperatorInput::PartiallyAppliedRight(Box::new(input.into_owned()))
            }
            BinaryOperatorInput::Applied(left, right) => BinaryOperatorInput::Applied(
                Box::new(left.into_owned()),
                Box::new(right.into_owned()),
            ),
        }
    }
}

impl<'src> VariadicOperatorInput<'src> {
    /// Produce a variadic operator input that owns its contents.
    pub fn into_owned(self) -> VariadicOperatorInput<'static> {
        match self {
            VariadicOperatorInput::Unapplied => VariadicOperatorInput::Unapplied,
            VariadicOperatorInput::Applied(contents) => VariadicOperatorInput::Applied(
                contents.into_iter().map(|node| node.into_owned()).collect(),
            ),
        }
    }
}
