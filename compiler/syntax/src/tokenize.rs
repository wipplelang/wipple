//! Convert a source file to a sequence of tokens.

#![allow(missing_docs)]

use crate::{Driver, Info};
use derivative::Derivative;
use itertools::Itertools;
use logos::Logos;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, hash::Hash, mem};
use ts_rs::TS;
use wipple_util::WithInfo;

/// A token in the source code.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
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

impl<'src> Token<'src> {
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
#[allow(missing_docs)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    strum::EnumString,
    strum::Display,
    Serialize,
    Deserialize,
    TS,
)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
#[ts(export)]
pub enum Keyword {
    #[strum(serialize = "_")]
    Underscore,

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

    #[strum(serialize = "infer")]
    Infer,
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
    Deserialize,
    TS,
)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
#[ts(export)]
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
#[allow(missing_docs)]
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
    Deserialize,
    TS,
)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
#[ts(export)]
pub enum VariadicOperator {
    #[strum(serialize = ";")]
    Tuple,

    #[strum(serialize = ",")]
    Collection,
}

/// A non-associative operator.
#[allow(missing_docs)]
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
    Deserialize,
    TS,
)]
#[serde(rename_all = "camelCase")]
#[strum(serialize_all = "kebab-case")]
#[ts(export)]
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
#[derive(Derivative, Serialize, Deserialize, TS)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq"),
    Hash(bound = "D::Info: Hash")
)]
#[serde(rename_all = "camelCase")]
#[ts(export, concrete(D = wipple_util::TsAny))]
pub enum Diagnostic<D: Driver> {
    /// The tokenizer encountered an invalid token.
    InvalidToken,

    /// The tokenizer expected one token, but a different one was found.
    Mismatch {
        /// The expected token, or `None` if the end of the file was expected.
        expected: Option<Token<'static>>,

        /// The provided token, or `None` if the end of the file was provided.
        found: Option<Token<'static>>,

        /// The token that [`found`] was supposed to match.
        matching: Option<WithInfo<D::Info, Token<'static>>>,
    },
}

#[derive(Debug, Logos)]
#[logos(skip r#"[\t ]+"#)]
enum RawToken<'src> {
    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("_", priority = 3)]
    Underscore,

    #[token("when")]
    When,

    #[token("where")]
    Where,

    #[token("type")]
    Type,

    #[token("trait")]
    Trait,

    #[token("instance")]
    Instance,

    #[token("intrinsic")]
    Intrinsic,

    #[token("as")]
    As,

    #[token("to")]
    To,

    #[token("by")]
    By,

    #[token("^")]
    Power,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token("%")]
    Remainder,

    #[token("+")]
    Add,

    #[token("-", priority = 3)]
    Subtract,

    #[token("<")]
    LessThan,

    #[token("<=")]
    LessThanOrEqual,

    #[token(">")]
    GreaterThan,

    #[token(">=")]
    GreaterThanOrEqual,

    #[token("=")]
    Equal,

    #[token("/=")]
    NotEqual,

    #[token("is")]
    Is,

    #[token("and")]
    And,

    #[token("or")]
    Or,

    #[token(".")]
    Apply,

    #[token("->")]
    Function,

    #[token(";")]
    Tuple,

    #[token(",")]
    Collection,

    #[token("=>")]
    TypeFunction,

    #[token(":")]
    Assign,

    #[token("::")]
    Annotate,

    #[regex(r#"\n"#)]
    LineBreak,

    #[regex(r#"--.*"#, |lex| Cow::Borrowed(&lex.slice()[2..]))]
    Comment(Cow<'src, str>),

    #[regex(r#"[A-Za-z0-9\-_]+[?]?"#, |lex| Cow::Borrowed(lex.slice()))]
    Name(Cow<'src, str>),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*"|'[^'\\]*(?s:\\.[^'\\]*)*'"#, |lex| Cow::Borrowed(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Text(Cow<'src, str>),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| Cow::Borrowed(lex.slice()), priority = 3)]
    Number(Cow<'src, str>),
}

/// Tokenize a source file.
pub fn tokenize<'a, 'src: 'a, D: Driver>(
    driver: &'a D,
    s: &'src str,
) -> impl Iterator<Item = Result<WithInfo<D::Info, Token<'src>>, WithInfo<D::Info, Diagnostic<D>>>> + 'a
where
    D::Info: From<Info>,
{
    logos::Lexer::new(s).spanned().map(|(result, span)| {
        let span = (span.start as u32)..(span.end as u32);

        match result {
            Ok(raw_token) => {
                let kind = match raw_token {
                    RawToken::LeftParenthesis => Token::LeftParenthesis,
                    RawToken::RightParenthesis => Token::RightParenthesis,
                    RawToken::LeftBracket => Token::LeftBracket,
                    RawToken::RightBracket => Token::RightBracket,
                    RawToken::LeftBrace => Token::LeftBrace,
                    RawToken::RightBrace => Token::RightBrace,
                    RawToken::Underscore => Token::Keyword(Keyword::Underscore),
                    RawToken::When => Token::Keyword(Keyword::When),
                    RawToken::Type => Token::Keyword(Keyword::Type),
                    RawToken::Trait => Token::Keyword(Keyword::Trait),
                    RawToken::Instance => Token::Keyword(Keyword::Instance),
                    RawToken::Intrinsic => Token::Keyword(Keyword::Intrinsic),
                    RawToken::As => Token::Operator(Operator::As),
                    RawToken::To => Token::Operator(Operator::To),
                    RawToken::By => Token::Operator(Operator::By),
                    RawToken::Power => Token::Operator(Operator::Power),
                    RawToken::Multiply => Token::Operator(Operator::Multiply),
                    RawToken::Divide => Token::Operator(Operator::Divide),
                    RawToken::Remainder => Token::Operator(Operator::Remainder),
                    RawToken::Add => Token::Operator(Operator::Add),
                    RawToken::Subtract => Token::Operator(Operator::Subtract),
                    RawToken::LessThan => Token::Operator(Operator::LessThan),
                    RawToken::LessThanOrEqual => Token::Operator(Operator::LessThanOrEqual),
                    RawToken::GreaterThan => Token::Operator(Operator::GreaterThan),
                    RawToken::GreaterThanOrEqual => Token::Operator(Operator::GreaterThanOrEqual),
                    RawToken::Equal => Token::Operator(Operator::Equal),
                    RawToken::NotEqual => Token::Operator(Operator::NotEqual),
                    RawToken::Is => Token::Operator(Operator::Is),
                    RawToken::And => Token::Operator(Operator::And),
                    RawToken::Or => Token::Operator(Operator::Or),
                    RawToken::Apply => Token::Operator(Operator::Apply),
                    RawToken::Function => Token::Operator(Operator::Function),
                    RawToken::Tuple => Token::VariadicOperator(VariadicOperator::Tuple),
                    RawToken::Collection => Token::VariadicOperator(VariadicOperator::Collection),
                    RawToken::Where => Token::NonAssociativeOperator(NonAssociativeOperator::Where),
                    RawToken::TypeFunction => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::TypeFunction)
                    }
                    RawToken::Assign => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::Assign)
                    }
                    RawToken::Annotate => {
                        Token::NonAssociativeOperator(NonAssociativeOperator::Annotate)
                    }
                    RawToken::LineBreak => Token::LineBreak,
                    RawToken::Comment(comment) => Token::Comment(comment),
                    RawToken::Name(name) => {
                        if let Ok(keyword) = name.parse::<Keyword>() {
                            Token::Keyword(keyword)
                        } else if let Ok(operator) = name.parse::<Operator>() {
                            Token::Operator(operator)
                        } else if let Ok(operator) = name.parse::<VariadicOperator>() {
                            Token::VariadicOperator(operator)
                        } else if let Ok(operator) = name.parse::<NonAssociativeOperator>() {
                            Token::NonAssociativeOperator(operator)
                        } else {
                            Token::Name(name)
                        }
                    }
                    RawToken::Text(text) => Token::Text(text),
                    RawToken::Number(number) => Token::Number(number),
                };

                Ok(WithInfo {
                    info: Info {
                        path: driver.file_path(),
                        visible_path: driver.visible_path(),
                        span,
                    }
                    .into(),
                    item: kind,
                })
            }
            Err(()) => Err(WithInfo {
                info: Info {
                    path: driver.file_path(),
                    visible_path: driver.visible_path(),
                    span,
                }
                .into(),
                item: Diagnostic::InvalidToken,
            }),
        }
    })
}

/// Format the output of [`tokenize`] to a string.
pub fn format<'a, 'src: 'a>(tokens: impl IntoIterator<Item = &'a Token<'src>>) -> String {
    let mut s = String::new();
    let mut line_indent = 0u32;
    let mut operator_indent = 0u32;
    let mut pad = true;
    let mut first_in_group = false;

    macro_rules! increment {
        ($indent:ident) => {
            $indent += 1;
        };
    }

    macro_rules! decrement {
        ($indent:ident) => {
            $indent = $indent.saturating_sub(1);
        };
    }

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token {
            Token::LeftParenthesis => {
                if pad {
                    s.push(' ');
                }

                s.push('(');
                increment!(line_indent);
                decrement!(operator_indent);
                pad = false;
                first_in_group = true;
            }
            Token::RightParenthesis => {
                s.push(')');
                decrement!(line_indent);
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::LeftBracket => {
                if pad {
                    s.push(' ');
                }

                s.push('[');
                increment!(line_indent);
                decrement!(operator_indent);
                pad = false;
                first_in_group = true;
            }
            Token::RightBracket => {
                s.push(']');
                decrement!(line_indent);
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::LeftBrace => {
                if pad {
                    s.push(' ');
                }

                s.push('{');
                increment!(line_indent);
                decrement!(operator_indent);
                pad = false;
                first_in_group = true;
            }
            Token::RightBrace => {
                s.push('}');
                decrement!(line_indent);
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::LineBreak => {
                let multiple = tokens
                    .next_if(|token| matches!(token, Token::LineBreak))
                    .is_some();

                if multiple {
                    while let Some(Token::LineBreak) = tokens.peek() {
                        tokens.next();
                    }
                }

                match tokens.peek() {
                    Some(Token::Operator(_)) => {
                        increment!(operator_indent);
                    }
                    Some(Token::RightParenthesis | Token::RightBracket | Token::RightBrace) => {
                        decrement!(line_indent);
                    }
                    _ => {}
                }

                if multiple {
                    s.push('\n');
                }

                s.push('\n');
                for _ in 0..(line_indent + operator_indent) {
                    s.push_str("  ");
                }

                pad = false;
                first_in_group = false;
                decrement!(operator_indent);
            }
            Token::Comment(comment) => {
                if pad || first_in_group {
                    s.push(' ');
                }

                s.push_str("--");
                s.push_str(comment);
            }
            Token::Keyword(keyword) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(&keyword.to_string());
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::Operator(operator) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(&operator.to_string());
                increment!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::VariadicOperator(operator) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(&operator.to_string());
                increment!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::NonAssociativeOperator(operator) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(&operator.to_string());
                increment!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::Name(name) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(name);
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::Text(text) => {
                if pad {
                    s.push(' ');
                }

                s.push('"');
                s.push_str(text);
                s.push('"');
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
            Token::Number(number) => {
                if pad {
                    s.push(' ');
                }

                s.push_str(number);
                decrement!(operator_indent);
                pad = true;
                first_in_group = false;
            }
        }
    }

    s.trim().to_string()
}

/// Get the comments associated with an offset in a source file. The offset must
/// point to the start of a token.
pub fn associated_comments<'src, D: Driver, I>(
    tokens: I,
    offset: u32,
) -> impl Iterator<Item = Cow<'src, str>>
where
    for<'a> &'a Info: From<&'a D::Info>,
    I: IntoIterator<Item = WithInfo<D::Info, Token<'src>>>,
    I::IntoIter: DoubleEndedIterator,
{
    tokens
        .into_iter()
        .rev()
        .peekable()
        .skip_while(move |token| <&Info>::from(&token.info).span.end > offset)
        .map_while(|token| match token.item {
            Token::Comment(comment) => Some(comment),
            _ => None,
        })
}

pub fn to_logical_lines<'a, 'src: 'a, D: Driver>(
    _driver: &'a D,
    tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>> + 'a,
) -> Vec<WithInfo<D::Info, Token<'src>>> {
    let mut result = Vec::new();

    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        match &token.item {
            Token::Comment(_) => {}
            Token::Operator(_) => {
                result.push(token);

                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            Token::LineBreak => {
                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }

                if let Some(operator) =
                    tokens.next_if(|token| matches!(token.item, Token::Operator(_)))
                {
                    result.push(operator);
                } else {
                    result.push(token);
                }

                while let Some(WithInfo {
                    item: Token::LineBreak,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                }
            }
            _ => {
                result.push(token);
            }
        }
    }

    result
}

/// A token tree.
#[allow(missing_docs)]
#[derive(Derivative)]
#[derivative(
    Debug(bound = ""),
    Clone(bound = ""),
    PartialEq(bound = "D::Info: PartialEq"),
    Eq(bound = "D::Info: Eq"),
    Hash(bound = "D::Info: Hash")
)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum TokenTree<'src, D: Driver> {
    Error,
    List(ListDelimiter, Vec<WithInfo<D::Info, TokenTree<'src, D>>>),
    Block(Vec<WithInfo<D::Info, TokenTree<'src, D>>>),
    Operator(
        WithInfo<D::Info, Operator>,
        WithInfo<D::Info, Box<TokenTree<'src, D>>>,
        WithInfo<D::Info, Box<TokenTree<'src, D>>>,
    ),
    VariadicOperator(
        WithInfo<D::Info, VariadicOperator>,
        Vec<WithInfo<D::Info, TokenTree<'src, D>>>,
    ),
    NonAssociativeOperator(
        WithInfo<D::Info, NonAssociativeOperator>,
        WithInfo<D::Info, Box<TokenTree<'src, D>>>,
        WithInfo<D::Info, Box<TokenTree<'src, D>>>,
    ),
    #[doc(hidden)]
    UnresolvedOperator(Operator),
    #[doc(hidden)]
    UnresolvedVariadicOperator(VariadicOperator),
    #[doc(hidden)]
    UnresolvedNonAssociativeOperator(NonAssociativeOperator),
    Keyword(Keyword),
    Name(Cow<'src, str>),
    Text(Cow<'src, str>),
    Number(Cow<'src, str>),
}

/// Whether a list is delimited by parentheses or brackets.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ListDelimiter {
    Parentheses,
    Brackets,
}

impl ListDelimiter {
    fn start(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::LeftParenthesis,
            ListDelimiter::Brackets => Token::LeftBracket,
        }
    }

    fn end(&self) -> Token<'static> {
        match self {
            ListDelimiter::Parentheses => Token::RightParenthesis,
            ListDelimiter::Brackets => Token::RightBracket,
        }
    }

    fn match_end<D: Driver>(
        &self,
        start_info: &D::Info,
        end: WithInfo<D::Info, &Token<'_>>,
    ) -> Result<(), WithInfo<D::Info, Diagnostic<D>>> {
        match (self, end.item) {
            (ListDelimiter::Parentheses, Token::RightParenthesis) => Ok(()),
            (ListDelimiter::Brackets, Token::RightBracket) => Ok(()),
            (_, item) => Err(WithInfo {
                info: end.info,
                item: Diagnostic::Mismatch {
                    expected: Some(self.end()),
                    found: Some(item.clone().into_owned()),
                    matching: Some(WithInfo {
                        info: start_info.clone(),
                        item: self.start(),
                    }),
                },
            }),
        }
    }
}

impl<'src, D: Driver> TokenTree<'src, D> {
    pub fn from_top_level(
        driver: &D,
        tokens: impl IntoIterator<Item = WithInfo<D::Info, Token<'src>>>,
    ) -> (
        WithInfo<D::Info, Self>,
        Vec<WithInfo<D::Info, Diagnostic<D>>>,
    )
    where
        D::Info: From<Info>,
    {
        fn parse_operators<'src, D: Driver>(
            delimiter: ListDelimiter,
            expressions: Vec<WithInfo<D::Info, TokenTree<'src, D>>>,
            diagnostics: &mut Vec<WithInfo<D::Info, Diagnostic<D>>>,
        ) -> TokenTree<'src, D> {
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
            enum AnyOperator {
                Operator(Operator),
                VariadicOperator(VariadicOperator),
                NonAssociativeOperator(NonAssociativeOperator),
            }

            let operators = expressions
                .iter()
                .enumerate()
                .filter_map(|(index, token)| match token.item {
                    TokenTree::UnresolvedOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::Operator(operator),
                        },
                    )),
                    TokenTree::UnresolvedVariadicOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::VariadicOperator(operator),
                        },
                    )),
                    TokenTree::UnresolvedNonAssociativeOperator(operator) => Some((
                        index,
                        WithInfo {
                            info: token.info.clone(),
                            item: AnyOperator::NonAssociativeOperator(operator),
                        },
                    )),
                    _ => None,
                })
                .max_set_by_key(|(_, operator)| operator.item);

            if operators.is_empty() {
                return TokenTree::List(delimiter, expressions);
            }

            fn tree<'src, D: Driver>(
                index: WithInfo<D::Info, usize>,
                mut expressions: Vec<WithInfo<<D as Driver>::Info, TokenTree<'src, D>>>,
                diagnostics: &mut Vec<WithInfo<<D as Driver>::Info, Diagnostic<D>>>,
            ) -> Option<(
                WithInfo<D::Info, TokenTree<'src, D>>,
                WithInfo<D::Info, TokenTree<'src, D>>,
            )> {
                let info = index.info;
                let index = index.item;

                match (index > 0, index + 1 < expressions.len()) {
                    (true, true) => {
                        let right = expressions.split_off(index + 1);
                        expressions.pop().unwrap();
                        let left = expressions;

                        let left_info = D::merge_info(
                            left.first().unwrap().info.clone(),
                            left.last().unwrap().info.clone(),
                        );

                        let right_info = D::merge_info(
                            right.first().unwrap().info.clone(),
                            right.last().unwrap().info.clone(),
                        );

                        Some((
                            WithInfo {
                                info: left_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    left,
                                    diagnostics,
                                ),
                            },
                            WithInfo {
                                info: right_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    right,
                                    diagnostics,
                                ),
                            },
                        ))
                    }
                    (true, false) => {
                        expressions.pop().unwrap();
                        let left = expressions;

                        let left_info = D::merge_info(
                            left.first().unwrap().info.clone(),
                            left.last().unwrap().info.clone(),
                        );

                        Some((
                            WithInfo {
                                info: left_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    left,
                                    diagnostics,
                                ),
                            },
                            WithInfo {
                                info,
                                item: TokenTree::Error,
                            },
                        ))
                    }
                    (false, true) => {
                        let right = expressions.split_off(1);

                        let right_info = D::merge_info(
                            right.first().unwrap().info.clone(),
                            right.last().unwrap().info.clone(),
                        );

                        Some((
                            WithInfo {
                                info,
                                item: TokenTree::Error,
                            },
                            WithInfo {
                                info: right_info,
                                item: parse_operators::<D>(
                                    ListDelimiter::Parentheses,
                                    right,
                                    diagnostics,
                                ),
                            },
                        ))
                    }
                    (false, false) => None,
                }
            }

            let operator = &operators.first().unwrap().1;
            let info = operator.info.clone();
            match operator.item {
                AnyOperator::Operator(operator) => {
                    let index = {
                        let (index, operator) = match operator.associativity() {
                            Associativity::Left => operators.first().unwrap(),
                            Associativity::Right => operators.last().unwrap(),
                        };

                        WithInfo {
                            info: operator.info.clone(),
                            item: *index,
                        }
                    };

                    match tree(index, expressions, diagnostics) {
                        Some((left, right)) => TokenTree::Operator(
                            WithInfo {
                                info,
                                item: operator,
                            },
                            left.boxed(),
                            right.boxed(),
                        ),
                        None => TokenTree::Error,
                    }
                }
                AnyOperator::VariadicOperator(operator) => {
                    let mut indices = operators.iter().map(|&(index, _)| index).peekable();

                    TokenTree::VariadicOperator(
                        WithInfo {
                            info: info.clone(),
                            item: operator,
                        },
                        expressions
                            .into_iter()
                            .enumerate()
                            .filter(|&(index, _)| operators.iter().all(|&(i, _)| i != index))
                            .group_by(|&(index, _)| match indices.peek().copied() {
                                Some(next) => {
                                    if next < index {
                                        true
                                    } else {
                                        indices.next();
                                        false
                                    }
                                }
                                None => true,
                            })
                            .into_iter()
                            .map(|(_, group)| {
                                let expressions =
                                    group.map(|(_, expression)| expression).collect::<Vec<_>>();

                                if expressions.is_empty() {
                                    return WithInfo {
                                        info: info.clone(),
                                        item: TokenTree::Error,
                                    };
                                }

                                let info = D::merge_info(
                                    expressions.first().unwrap().info.clone(),
                                    expressions.last().unwrap().info.clone(),
                                );

                                WithInfo {
                                    info,
                                    item: parse_operators::<D>(
                                        ListDelimiter::Parentheses,
                                        expressions,
                                        diagnostics,
                                    ),
                                }
                            })
                            .collect(),
                    )
                }
                AnyOperator::NonAssociativeOperator(operator) => {
                    if operators.len() != 1 {
                        return TokenTree::Error;
                    }

                    let index = {
                        let (index, operator) = operators.first().unwrap();

                        WithInfo {
                            info: operator.info.clone(),
                            item: *index,
                        }
                    };

                    match tree(index, expressions, diagnostics) {
                        Some((left, right)) => TokenTree::NonAssociativeOperator(
                            WithInfo {
                                info,
                                item: operator,
                            },
                            left.boxed(),
                            right.boxed(),
                        ),
                        None => TokenTree::Error,
                    }
                }
            }
        }

        let mut tokens = tokens.into_iter();

        let first_token = match tokens.next() {
            Some(token) => token,
            None => {
                return (
                    WithInfo {
                        info: Info {
                            path: driver.file_path(),
                            visible_path: driver.visible_path(),
                            span: 0..0,
                        }
                        .into(),
                        item: TokenTree::Block(Vec::new()),
                    },
                    Vec::new(),
                )
            }
        };

        let first_info = first_token.info.clone();
        let mut info = first_info.clone();

        let mut stack = vec![(info, TokenTree::Block(Vec::new()))];
        let mut diagnostics = Vec::new();
        for token in [first_token].into_iter().chain(tokens.by_ref()) {
            info = token.info;

            macro_rules! push {
                ($item:expr) => {{
                    let group = match stack.last_mut() {
                        Some((_, TokenTree::List(_, expressions))) => expressions,
                        Some((begin_info, TokenTree::Block(statements))) => {
                            if statements.is_empty() {
                                statements.push(WithInfo {
                                    info: begin_info.clone(),
                                    item: TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                                });
                            }

                            match &mut statements.last_mut().unwrap().item {
                                TokenTree::List(_, expressions) => expressions,
                                _ => unreachable!(),
                            }
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });

                            continue;
                        }
                    };

                    group.push($item);
                }};
            }

            match token.item {
                Token::LeftParenthesis => {
                    stack.push((
                        info,
                        TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                    ));
                }
                Token::LeftBracket => {
                    stack.push((info, TokenTree::List(ListDelimiter::Brackets, Vec::new())));
                }
                Token::RightParenthesis | Token::RightBracket => {
                    let (begin_info, delimiter, expressions) = match stack.pop() {
                        Some((begin_info, TokenTree::List(delimiter, expressions))) => {
                            if let Err(diagnostic) = delimiter.match_end(
                                &begin_info,
                                WithInfo {
                                    info: info.clone(),
                                    item: &token.item,
                                },
                            ) {
                                diagnostics.push(diagnostic);
                            }

                            (begin_info, delimiter, expressions)
                        }
                        Some((begin_info, TokenTree::Block(_))) => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: Some(Token::RightBrace),
                                    found: Some(token.item.into_owned()),
                                    matching: Some(WithInfo {
                                        info: begin_info,
                                        item: Token::LeftBrace,
                                    }),
                                },
                            });

                            continue;
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });

                            continue;
                        }
                    };

                    push!(WithInfo {
                        info: D::merge_info(begin_info, info),
                        item: parse_operators::<D>(delimiter, expressions, &mut diagnostics),
                    });
                }
                Token::LeftBrace => {
                    stack.push((info, TokenTree::Block(Vec::new())));
                }
                Token::RightBrace => {
                    let (begin_info, mut statements) = match stack.pop() {
                        Some((begin_info, TokenTree::Block(statements))) => {
                            (begin_info, statements)
                        }
                        Some((begin_info, TokenTree::List(delimiter, _))) => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: Some(delimiter.end()),
                                    found: Some(token.item.into_owned()),
                                    matching: Some(WithInfo {
                                        info: begin_info,
                                        item: delimiter.start(),
                                    }),
                                },
                            });

                            continue;
                        }
                        _ => {
                            diagnostics.push(WithInfo {
                                info,
                                item: Diagnostic::Mismatch {
                                    expected: None,
                                    found: Some(token.item.into_owned()),
                                    matching: None,
                                },
                            });

                            continue;
                        }
                    };

                    // Allow line break before closing brace
                    if statements.last().is_some_and(|tree| {
                        matches!(&tree.item, TokenTree::List(_, elements) if elements.is_empty())
                    }) {
                        statements.pop().unwrap();
                    }

                    push!(WithInfo {
                        info: D::merge_info(begin_info, info),
                        item: TokenTree::Block(statements),
                    });
                }
                Token::LineBreak => match stack.last_mut() {
                    Some((_, TokenTree::List(_, _))) => continue,
                    Some((begin_info, TokenTree::Block(statements))) => {
                        if let Some(tree) = statements.last_mut() {
                            let (delimiter, expressions) = match &mut tree.item {
                                TokenTree::List(delimiter, expressions) => {
                                    (*delimiter, mem::take(expressions))
                                }
                                _ => unreachable!(),
                            };

                            if let Some(first) = expressions.first() {
                                let last = expressions.last().unwrap();
                                tree.info = D::merge_info(first.info.clone(), last.info.clone());
                            }

                            tree.item =
                                parse_operators::<D>(delimiter, expressions, &mut diagnostics);
                        }

                        statements.push(WithInfo {
                            info: begin_info.clone(),
                            item: TokenTree::List(ListDelimiter::Parentheses, Vec::new()),
                        });
                    }
                    _ => {
                        diagnostics.push(WithInfo {
                            info,
                            item: Diagnostic::Mismatch {
                                expected: None,
                                found: Some(token.item.into_owned()),
                                matching: None,
                            },
                        });
                    }
                },
                Token::Comment(_) => continue,
                Token::Keyword(keyword) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Keyword(keyword),
                    });
                }
                Token::Operator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedOperator(operator),
                    });
                }
                Token::VariadicOperator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedVariadicOperator(operator),
                    });
                }
                Token::NonAssociativeOperator(operator) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::UnresolvedNonAssociativeOperator(operator),
                    });
                }
                Token::Name(ref name) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Name(name.clone()),
                    });
                }
                Token::Text(ref text) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Text(text.clone()),
                    });
                }
                Token::Number(ref number) => {
                    push!(WithInfo {
                        info,
                        item: TokenTree::Number(number.clone()),
                    });
                }
            }
        }

        let mut tree = match stack.pop() {
            Some((begin_info, tree)) => {
                if let Some((begin_info, tree)) = stack.pop() {
                    let end_token = match tree {
                        TokenTree::List(delimiter, _) => delimiter.end(),
                        TokenTree::Block(_) => Token::RightBrace,
                        _ => unreachable!(),
                    };

                    let (info, token) = match tokens.next() {
                        Some(token) => (token.info, Some(token.item.into_owned())),
                        None => (first_info, None),
                    };

                    diagnostics.push(WithInfo {
                        info,
                        item: Diagnostic::Mismatch {
                            expected: None,
                            found: token,
                            matching: Some(WithInfo {
                                info: begin_info,
                                item: end_token,
                            }),
                        },
                    });
                }

                WithInfo {
                    info: begin_info,
                    item: tree,
                }
            }
            None => {
                let (info, token) = match tokens.next() {
                    Some(token) => (token.info, Some(token.item.into_owned())),
                    None => (first_info.clone(), None),
                };

                diagnostics.push(WithInfo {
                    info,
                    item: Diagnostic::Mismatch {
                        expected: None,
                        found: token,
                        matching: None,
                    },
                });

                WithInfo {
                    info: first_info,
                    item: TokenTree::Error,
                }
            }
        };

        // Allow trailing line break
        if let TokenTree::Block(statements) = &mut tree.item {
            if statements.last().is_some_and(
                |tree| matches!(&tree.item, TokenTree::List(_, elements) if elements.is_empty()),
            ) {
                statements.pop().unwrap();
            }
        }

        (tree, diagnostics)
    }
}