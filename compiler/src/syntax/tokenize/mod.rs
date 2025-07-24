//! Convert a source file to a sequence of tokens.

#![allow(missing_docs)]

mod format;
mod logical_lines;
mod raw;
mod tree;

pub use format::format;
pub use logical_lines::to_logical_lines;
pub use raw::tokenize;

use crate::util::WithInfo;
use serde::Serialize;
use std::{borrow::Cow, hash::Hash};

/// A token in the source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum Token<'src> {
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LineBreak,
    Comment(Cow<'src, str>),
    Keyword(Keyword),
    Operator(Operator),
    VariadicOperator(VariadicOperator),
    NonAssociativeOperator(NonAssociativeOperator),
    Name(Cow<'src, str>),
    Text(Cow<'src, str>),
    Number(Cow<'src, str>),
}

impl Token<'_> {
    /// Clone the strings referenced by this token.
    pub fn into_owned(self) -> Token<'static> {
        match self {
            Token::LeftParenthesis => Token::LeftParenthesis,
            Token::RightParenthesis => Token::RightParenthesis,
            Token::LeftBracket => Token::LeftBracket,
            Token::RightBracket => Token::RightBracket,
            Token::LeftBrace => Token::LeftBrace,
            Token::RightBrace => Token::RightBrace,
            Token::LineBreak => Token::LineBreak,
            Token::Comment(comment) => Token::Comment(comment.into_owned().into()),
            Token::Keyword(keyword) => Token::Keyword(keyword),
            Token::Operator(operator) => Token::Operator(operator),
            Token::VariadicOperator(operator) => Token::VariadicOperator(operator),
            Token::NonAssociativeOperator(operator) => Token::NonAssociativeOperator(operator),
            Token::Name(name) => Token::Name(name.into_owned().into()),
            Token::Text(text) => Token::Text(text.into_owned().into()),
            Token::Number(number) => Token::Number(number.into_owned().into()),
        }
    }
}

/// A keyword.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumString, strum::Display, Serialize)]
#[strum(serialize_all = "kebab-case")]
pub enum Keyword {
    #[strum(serialize = "_")]
    Underscore,

    #[strum(serialize = "do")]
    Do,

    #[strum(serialize = "@")]
    Attribute,

    #[strum(serialize = "!")]
    Mutate,

    #[strum(serialize = "when")]
    When,

    #[strum(serialize = "type")]
    Type,

    #[strum(serialize = "trait")]
    Trait,

    #[strum(serialize = "instance")]
    Instance,

    #[strum(serialize = "intrinsic")]
    Intrinsic,
}

impl Keyword {
    fn is_prefix(&self) -> bool {
        matches!(self, Keyword::Attribute)
    }

    fn is_suffix(&self) -> bool {
        matches!(self, Keyword::Mutate)
    }
}

/// A binary operator.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::Display,
    strum::EnumDiscriminants,
    Serialize,
)]
#[strum(serialize_all = "kebab-case")]
pub enum Operator {
    #[strum(serialize = "as")]
    As,

    #[strum(serialize = "to")]
    To,

    #[strum(serialize = "by")]
    By,

    #[strum(serialize = "^")]
    Power,

    #[strum(serialize = "*")]
    Multiply,

    #[strum(serialize = "/")]
    Divide,

    #[strum(serialize = "%")]
    Remainder,

    #[strum(serialize = "+")]
    Add,

    #[strum(serialize = "-")]
    Subtract,

    #[strum(serialize = "<")]
    LessThan,

    #[strum(serialize = "<=")]
    LessThanOrEqual,

    #[strum(serialize = ">")]
    GreaterThan,

    #[strum(serialize = ">=")]
    GreaterThanOrEqual,

    #[strum(serialize = "=")]
    Equal,

    #[strum(serialize = "/=")]
    NotEqual,

    #[strum(serialize = "is")]
    Is,

    #[strum(serialize = "and")]
    And,

    #[strum(serialize = "or")]
    Or,

    #[strum(serialize = ".")]
    Apply,

    #[strum(serialize = "->")]
    Function,
}

/// A variadic operator.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::Display,
    strum::EnumDiscriminants,
    Serialize,
)]
#[strum(serialize_all = "kebab-case")]
pub enum VariadicOperator {
    #[strum(serialize = ";")]
    Tuple,

    #[strum(serialize = ",")]
    Collection,
}

/// A non-associative operator.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    strum::EnumString,
    strum::Display,
    strum::EnumDiscriminants,
    Serialize,
)]
#[strum(serialize_all = "kebab-case")]
pub enum NonAssociativeOperator {
    #[strum(serialize = "where")]
    Where,

    #[strum(serialize = "=>")]
    TypeFunction,

    #[strum(serialize = "::")]
    Annotate,

    #[strum(serialize = ":")]
    Assign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Associativity {
    Left,
    Right,
}

impl Operator {
    fn associativity(&self) -> Associativity {
        match self {
            Operator::As => Associativity::Left,
            Operator::To => Associativity::Left,
            Operator::By => Associativity::Left,
            Operator::Power => Associativity::Right,
            Operator::Multiply => Associativity::Left,
            Operator::Divide => Associativity::Left,
            Operator::Remainder => Associativity::Left,
            Operator::Add => Associativity::Left,
            Operator::Subtract => Associativity::Left,
            Operator::LessThan => Associativity::Left,
            Operator::LessThanOrEqual => Associativity::Left,
            Operator::GreaterThan => Associativity::Left,
            Operator::GreaterThanOrEqual => Associativity::Left,
            Operator::Equal => Associativity::Left,
            Operator::NotEqual => Associativity::Left,
            Operator::Is => Associativity::Left,
            Operator::And => Associativity::Left,
            Operator::Or => Associativity::Left,
            Operator::Apply => Associativity::Left,
            Operator::Function => Associativity::Right,
        }
    }
}

/// An error occurring during [`tokenize`] or [`read`].
#[derive(Debug, Hash)]
pub enum Diagnostic {
    /// The tokenizer encountered an invalid token.
    InvalidToken,

    /// The tokenizer encountered an empty pair of parentheses.
    EmptyParentheses,

    /// The tokenizer encountered an empty pair of braces.
    EmptyBraces,

    /// The tokenizer expected one token, but a different one was found.
    Mismatch {
        /// The expected token, or `None` if the end of the file was expected.
        expected: Option<Token<'static>>,

        /// The provided token, or `None` if the end of the file was provided.
        found: Option<Token<'static>>,

        /// The token that [`found`] was supposed to match.
        matching: Option<WithInfo<Token<'static>>>,
    },
}

/// A token tree, ie. a concrete syntax tree that does grouping by parentheses
/// and operators, but has no further syntactic structure.
#[derive(Debug, Hash)]
pub enum TokenTree<'src> {
    Error,
    EmptyFile,
    List(ListDelimiter, Vec<WithInfo<TokenTree<'src>>>),
    Block(Vec<WithInfo<TokenTree<'src>>>),
    Operator(
        WithInfo<Operator>,
        WithInfo<Box<TokenTree<'src>>>,
        WithInfo<Box<TokenTree<'src>>>,
    ),
    VariadicOperator(WithInfo<VariadicOperator>, Vec<WithInfo<TokenTree<'src>>>),
    NonAssociativeOperator(
        WithInfo<NonAssociativeOperator>,
        WithInfo<Box<TokenTree<'src>>>,
        WithInfo<Box<TokenTree<'src>>>,
    ),
    #[doc(hidden)]
    UnresolvedOperator(Operator),
    #[doc(hidden)]
    UnresolvedVariadicOperator(VariadicOperator),
    #[doc(hidden)]
    UnresolvedNonAssociativeOperator(NonAssociativeOperator),
    #[doc(hidden)]
    UnresolvedAttribute(Option<WithInfo<Box<TokenTree<'src>>>>),
    Attribute(
        WithInfo<Box<TokenTree<'src>>>,
        WithInfo<Box<TokenTree<'src>>>,
    ),
    Mutate(WithInfo<Box<TokenTree<'src>>>),
    Keyword(Keyword),
    Name(Cow<'src, str>),
    Text(Cow<'src, str>),
    Number(Cow<'src, str>),
}

/// Whether a list is delimited by parentheses or brackets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ListDelimiter {
    Parentheses,
    Brackets,
}
