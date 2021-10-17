use crate::diagnostics::{Diagnostic, DiagnosticLevel, Diagnostics, Note};
use internment::Intern;
use lazy_static::lazy_static;
use logos::{Lexer, Logos, SpannedIter};
use regex::Regex;
use rust_decimal::prelude::*;
use serde::Serialize;
use std::{fmt, iter::Peekable, ops::Range, path::PathBuf};

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub file: Intern<PathBuf>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: Intern<PathBuf>, range: Range<usize>) -> Self {
        Span {
            file,
            start: range.start,
            end: range.end,
        }
    }

    pub fn with_start(self, start: usize) -> Self {
        Span { start, ..self }
    }

    pub fn with_end(self, end: usize) -> Self {
        Span { end, ..self }
    }

    pub fn offset(self, range: usize) -> Self {
        Span {
            start: self.start + range,
            end: self.end + range,
            ..self
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} @ {}..{}",
            self.file.to_string_lossy(),
            self.start,
            self.end
        )
    }
}

#[derive(Serialize)]
pub struct File<'src> {
    pub span: Span,
    pub shebang: Option<&'src str>,
    pub statements: Vec<Expr>,
}

#[derive(Serialize)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Serialize)]
pub enum ExprKind {
    Name(Intern<String>),
    Text(Intern<String>),
    Number(Intern<Decimal>),
    Quote(Box<Expr>),
    List(Vec<Expr>),
    Attribute(Vec<Expr>),
    Block(Vec<Expr>),
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }
}

impl From<File<'_>> for Expr {
    fn from(file: File) -> Self {
        Expr::new(file.span, ExprKind::Block(file.statements))
    }
}

pub fn parse<'src>(
    file: Intern<PathBuf>,
    code: &'src str,
    diagnostics: &mut Diagnostics,
) -> Option<File<'src>> {
    let (shebang, code) = match parse_shebang(code) {
        Some((shebang, code)) => (Some(shebang), code),
        None => (None, code),
    };

    let mut parser = Parser {
        lexer: Lexer::new(code).spanned().peekable(),
        len: code.len(),
        offset: shebang.map(|s| "#!".len() + s.len()).unwrap_or(0),
        file,
        diagnostics,
    };

    parser.parse_file().map(|statements| File {
        span: parser.file_span(),
        shebang,
        statements,
    })
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
enum Token<'src> {
    #[token("'")]
    Quote,

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

    #[regex(r#"\n+"#)]
    LineBreak,

    #[regex(r#"\t+"#, |lex| lex.slice().len() as u32)]
    Indent(u32),

    #[regex(r#" +"#, logos::skip)]
    Space,

    #[regex(r#"--.*"#, logos::skip, priority = 2)]
    Comment,

    #[regex(r#"[^\n\t \(\)\[\]\{\}'"]+"#, |lex| lex.slice())]
    Name(&'src str),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| &lex.slice()[1..(lex.slice().len() - 1)])]
    Text(&'src str),

    #[regex(r#"[0-9]+(\.[0-9]+)?"#, |lex| lex.slice(), priority = 2)]
    Number(&'src str),

    #[error]
    Error,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Quote => write!(f, "'"),
            Token::LeftParenthesis => write!(f, "'('"),
            Token::RightParenthesis => write!(f, "')'"),
            Token::LeftBracket => write!(f, "'['"),
            Token::RightBracket => write!(f, "']'"),
            Token::LeftBrace => write!(f, "'{{'"),
            Token::RightBrace => write!(f, "'}}'"),
            Token::LineBreak => write!(f, "line break"),
            Token::Indent(_) => write!(f, "indent"),
            Token::Space | Token::Comment => unreachable!(),
            Token::Text(text) => write!(f, "'\"{}\"'", text),
            Token::Number(number) => write!(f, "'{}'", number),
            Token::Name(name) => write!(f, "'{}'", name),
            Token::Error => write!(f, "invalid token"),
        }
    }
}

struct Parser<'a, 'src> {
    lexer: Peekable<SpannedIter<'src, Token<'src>>>,
    len: usize,
    offset: usize,
    file: Intern<PathBuf>,
    diagnostics: &'a mut Diagnostics,
}

#[derive(Debug, Clone, Copy)]
enum ParseError {
    WrongTokenType,
    InvalidExpr,
    EndOfFile,
}

impl<'src> Parser<'_, 'src> {
    fn parse_file(&mut self) -> Option<Vec<Expr>> {
        let (statements, _) = self.parse_statements(None)?;

        if let (span, Some(token)) = self.consume() {
            self.diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "Syntax error",
                vec![Note::primary(
                    span,
                    format!("Expected end of file, found {}", token),
                )],
            ));

            return None;
        }

        Some(statements)
    }

    fn parse_statements(&mut self, end_token: Option<Token>) -> Option<(Vec<Expr>, Span)> {
        let mut statements = Vec::<(u32, Vec<Expr>)>::new();
        let mut error = false;

        let end_span = loop {
            // Consume line breaks
            while let (_, Some(Token::LineBreak)) = self.peek() {
                self.consume();
            }

            // Consume indentation and count
            let indent = if let (_, Some(Token::Indent(indent))) = self.peek() {
                self.consume();
                indent
            } else {
                0
            };

            // Consume expressions
            let (mut exprs, parsed_end_token, end_span) = (|| {
                let mut exprs = Vec::new();
                loop {
                    let (span, token) = self.peek();

                    match (token, end_token) {
                        (Some(Token::LineBreak), _) => {
                            self.consume();
                            return (exprs, token, None);
                        }
                        (None, _) => {
                            self.consume();
                            return (exprs, token, Some(span));
                        }
                        (Some(token), Some(end_token)) if token == end_token => {
                            self.consume();
                            return (exprs, Some(token), Some(span));
                        }
                        _ => {
                            if let Some(expr) = self.parse_expr() {
                                exprs.push(expr);
                            } else {
                                error = true;
                                return (exprs, token, None);
                            }
                        }
                    }
                }
            })();

            if let Some(end_token) = end_token {
                if parsed_end_token.is_none() {
                    self.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "Syntax error",
                        vec![Note::primary(
                            self.eof_span(),
                            format!("Expected {}, found end of file", end_token),
                        )],
                    ));

                    return None;
                }
            }

            let last_indent = statements
                .last()
                .map(|(indent, ..)| *indent)
                .unwrap_or(indent);

            if indent <= last_indent {
                statements.push((indent, Vec::new()));
            }

            statements.last_mut().unwrap().1.append(&mut exprs);

            if let Some(end_span) = end_span {
                break end_span;
            }
        };

        (!error).then(|| {
            let statements = statements
                .into_iter()
                .filter_map(|(_, statement)| {
                    (!statement.is_empty()).then(|| {
                        let span = statement
                            .first()
                            .unwrap()
                            .span
                            .with_end(statement.last().unwrap().span.end);

                        Expr::new(span, ExprKind::List(statement))
                    })
                })
                .collect();

            (statements, end_span)
        })
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        macro_rules! parse_each {
            ($parse:ident $(, $rest:ident)* $(,)?) => {
                match self.$parse() {
                    Ok(expr) => Some(expr),
                    Err(ParseError::WrongTokenType) => parse_each!($($rest),*),
                    Err(ParseError::InvalidExpr) => None,
                    Err(ParseError::EndOfFile) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Syntax error",
                            vec![Note::primary(self.eof_span(), "Expected expression, found end of file")],
                        ));

                        None
                    }
                };
            };
            () => {{
                let (span, token) = self.consume();

                self.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "Syntax error",
                    vec![Note::primary(
                        span,
                        format!(
                            "Expected expression, found {}",
                            token
                                .map(|t| t.to_string())
                                .as_deref()
                                .unwrap_or("end of file")
                        ),
                    )],
                ));

                None
            }};
        }

        parse_each!(
            try_parse_name,
            try_parse_text,
            try_parse_number,
            try_parse_quote,
            try_parse_list,
            try_parse_attribute,
            try_parse_block,
        )
    }

    fn try_parse_name(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Name(name)) => {
                self.consume();

                Ok(Expr::new(span, ExprKind::Name(Intern::from(name))))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn try_parse_text(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Text(raw)) => {
                self.consume();

                let mut string = String::with_capacity(raw.len());
                let mut error = false;

                rustc_lexer::unescape::unescape_str(raw, &mut |range, result| match result {
                    Ok(ch) => string.push(ch),
                    Err(e) => {
                        error = true;

                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Syntax error",
                            vec![Note::primary(
                                Span::new(self.file, range).offset(span.start),
                                format!("Invalid character sequence in string literal ({:?})", e),
                            )],
                        ));
                    }
                });

                if error {
                    Err(ParseError::InvalidExpr)
                } else {
                    Ok(Expr::new(span, ExprKind::Text(Intern::from(&string))))
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn try_parse_number(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Number(raw)) => {
                self.consume();

                match raw.parse() {
                    Ok(number) => Ok(Expr::new(span, ExprKind::Number(Intern::new(number)))),
                    Err(error) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "Syntax error",
                            vec![Note::primary(span, error)],
                        ));

                        Err(ParseError::InvalidExpr)
                    }
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn try_parse_quote(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Quote) => {
                self.consume();

                match self.parse_expr() {
                    Some(expr) => Ok(Expr::new(
                        span.with_end(expr.span.end),
                        ExprKind::Quote(Box::new(expr)),
                    )),
                    None => Err(ParseError::InvalidExpr),
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn try_parse_list(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftParenthesis) => {
                self.consume();

                if let Some((exprs, end_span)) = self.parse_list_contents(Token::RightParenthesis) {
                    Ok(Expr::new(
                        span.with_end(end_span.end),
                        ExprKind::List(exprs),
                    ))
                } else {
                    Err(ParseError::InvalidExpr)
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn try_parse_attribute(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftBracket) => {
                self.consume();

                if let Some((exprs, end_span)) = self.parse_list_contents(Token::RightBracket) {
                    Ok(Expr::new(
                        span.with_end(end_span.end),
                        ExprKind::Attribute(exprs),
                    ))
                } else {
                    Err(ParseError::InvalidExpr)
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn parse_list_contents(&mut self, end_token: Token) -> Option<(Vec<Expr>, Span)> {
        let mut exprs = Vec::<Expr>::new();
        let mut error = false;

        let (parsed_end_token, end_span) = loop {
            // Consume line breaks and indentation
            while let (_, Some(Token::LineBreak | Token::Indent(_))) = self.peek() {
                self.consume();
            }

            // Consume expressions
            let end_span = loop {
                let (span, token) = self.peek();

                match token {
                    Some(Token::LineBreak) => {
                        self.consume();
                        break (token, None);
                    }
                    Some(token) if token == end_token => {
                        self.consume();
                        break (Some(token), Some(span));
                    }
                    None => {
                        self.consume();
                        break (token, Some(span));
                    }
                    _ => {
                        if let Some(expr) = self.parse_expr() {
                            exprs.push(expr);
                        } else {
                            error = true;
                            break (token, None);
                        }
                    }
                }
            };

            if let (token, Some(end_span)) = end_span {
                break (token, end_span);
            }
        };

        if parsed_end_token.is_none() {
            self.diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "Syntax error",
                vec![Note::primary(
                    self.eof_span(),
                    format!("Expected {}, found end of file", end_token),
                )],
            ));

            return None;
        }

        (!error).then(|| (exprs, end_span))
    }

    fn try_parse_block(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftBrace) => {
                self.consume();

                match self.parse_statements(Some(Token::RightBrace)) {
                    Some((statements, end_range)) => Ok(Expr::new(
                        span.with_end(end_range.end),
                        ExprKind::Block(statements),
                    )),
                    None => Err(ParseError::InvalidExpr),
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    fn peek(&mut self) -> (Span, Option<Token<'src>>) {
        match self.lexer.peek().cloned() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    fn consume(&mut self) -> (Span, Option<Token<'src>>) {
        match self.lexer.next() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    fn offset(&self, range: Range<usize>) -> Span {
        Span::new(
            self.file,
            (range.start + self.offset)..(range.end + self.offset),
        )
    }

    fn eof_span(&self) -> Span {
        self.offset(self.len..self.len)
    }

    fn file_span(&self) -> Span {
        self.offset(0..self.len)
    }
}

fn parse_shebang(code: &str) -> Option<(&str, &str)> {
    lazy_static! {
        static ref SHEBANG_REGEX: Regex = Regex::new(r#"^#!(.*)"#).unwrap();
    }

    SHEBANG_REGEX
        .find(code)
        .map(|m| (&m.as_str()[2..], &code[m.end()..]))
}
