use crate::{
    diagnostics::*,
    parse::{Span, Token},
    FilePath, InternedString,
};
use lazy_static::lazy_static;
use logos::SpannedIter;
use regex::Regex;
use rust_decimal::prelude::*;
use std::{iter::Peekable, ops::Range};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub shebang: Option<InternedString>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Name(InternedString),
    Text(InternedString),
    Number(Decimal),
    Quote(Box<Expr>),
    List(Vec<ListLine>),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub leading_lines: u32,
    pub indent: u32,
    pub lines: Vec<ListLine>,
}

#[derive(Debug, Clone)]
pub struct ListLine {
    pub exprs: Vec<Expr>,
    pub comment: Option<InternedString>,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }
}

pub struct Parser<'a, 'src> {
    pub lexer: Peekable<SpannedIter<'src, Token<'src>>>,
    pub len: usize,
    pub offset: usize,
    pub file: FilePath,
    pub diagnostics: &'a mut Diagnostics,
}

#[derive(Clone, Copy)]
pub enum ParseError {
    WrongTokenType,
    InvalidExpr,
    EndOfFile,
}

impl<'src> Parser<'_, 'src> {
    pub fn parse_file(&mut self) -> Option<Vec<Statement>> {
        let (statements, _) = self.parse_statements(None)?;

        if let (span, Some(token)) = self.consume() {
            self.diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "syntax error",
                vec![Note::primary(
                    span,
                    format!("expected end of file, found {}", token),
                )],
            ));

            return None;
        }

        Some(statements)
    }

    pub fn parse_statements(
        &mut self,
        end_token: Option<Token<'src>>,
    ) -> Option<(Vec<Statement>, Span)> {
        let mut statements = Vec::<Statement>::new();
        let mut error = false;

        let end_span = loop {
            // Consume line breaks
            let mut leading_lines = 0;
            while let (_, Some(Token::LineBreak)) = self.peek() {
                leading_lines += 1;
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
            let (exprs, parsed_end_token, end_span, comment) = (|| {
                let mut exprs = Vec::new();
                let mut comment = None;
                loop {
                    let (span, token) = self.peek();

                    match (token, end_token) {
                        (Some(Token::LineBreak), _) => {
                            self.consume();
                            return (exprs, token, None, comment);
                        }
                        (Some(Token::Comment(c)), _) => {
                            self.consume();
                            comment = Some(InternedString::new(c))
                        }
                        (None, _) => {
                            self.consume();
                            return (exprs, token, Some(span), comment);
                        }
                        (Some(token), Some(end_token)) if token == end_token => {
                            self.consume();
                            return (exprs, Some(token), Some(span), comment);
                        }
                        _ => {
                            if let Some(expr) = self.parse_expr() {
                                exprs.push(expr);
                            } else {
                                error = true;
                                return (exprs, token, None, comment);
                            }
                        }
                    }
                }
            })();

            if let Some(end_token) = end_token {
                if parsed_end_token.is_none() {
                    self.diagnostics.add(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "syntax error",
                        vec![Note::primary(
                            self.eof_span(),
                            format!("expected {}, found end of file", end_token),
                        )],
                    ));

                    return None;
                }
            }

            let last_indent = statements
                .last()
                .map(|statement| statement.indent)
                .unwrap_or(indent);

            if indent <= last_indent {
                statements.push(Statement {
                    leading_lines,
                    indent,
                    lines: Vec::new(),
                })
            }

            statements
                .last_mut()
                .unwrap()
                .lines
                .push(ListLine { exprs, comment });

            if let Some(end_span) = end_span {
                break end_span;
            }
        };

        (!error).then(|| (statements, end_span))
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        macro_rules! parse_each {
            ($parse:ident $(, $rest:ident)* $(,)?) => {
                match self.$parse() {
                    Ok(expr) => Some(expr),
                    Err(ParseError::WrongTokenType) => parse_each!($($rest),*),
                    Err(ParseError::InvalidExpr) => None,
                    Err(ParseError::EndOfFile) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "syntax error",
                            vec![Note::primary(self.eof_span(), "expected expression, found end of file")],
                        ));

                        None
                    }
                }
            };
            () => {{
                let (span, token) = self.consume();

                self.diagnostics.add(Diagnostic::new(
                    DiagnosticLevel::Error,
                    "syntax error",
                    vec![Note::primary(
                        span,
                        format!(
                            "expected expression, found {}",
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
            try_parse_block,
        )
    }

    pub fn try_parse_name(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Name(name)) => {
                self.consume();

                Ok(Expr::new(span, ExprKind::Name(InternedString::new(name))))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_text(&mut self) -> Result<Expr, ParseError> {
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
                            "syntax error",
                            vec![Note::primary(
                                Span::new(self.file, range).offset(span.start),
                                format!("invalid character sequence in string literal ({:?})", e),
                            )],
                        ));
                    }
                });

                if error {
                    Err(ParseError::InvalidExpr)
                } else {
                    Ok(Expr::new(span, ExprKind::Text(InternedString::new(string))))
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_number(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Number(raw)) => {
                self.consume();

                match raw.parse() {
                    Ok(number) => Ok(Expr::new(span, ExprKind::Number(number))),
                    Err(error) => {
                        self.diagnostics.add(Diagnostic::new(
                            DiagnosticLevel::Error,
                            "syntax error",
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

    pub fn try_parse_quote(&mut self) -> Result<Expr, ParseError> {
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

    pub fn try_parse_list(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftParenthesis) => {
                self.consume();

                if let Some((lines, end_span)) = self.parse_list_contents(Token::RightParenthesis) {
                    Ok(Expr::new(
                        span.with_end(end_span.end),
                        ExprKind::List(lines),
                    ))
                } else {
                    Err(ParseError::InvalidExpr)
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn parse_list_contents(&mut self, end_token: Token) -> Option<(Vec<ListLine>, Span)> {
        let mut lines = Vec::<ListLine>::new();
        let mut error = false;

        let (parsed_end_token, end_span) = loop {
            // Consume line breaks and indentation
            while let (_, Some(Token::LineBreak | Token::Indent(_))) = self.peek() {
                self.consume();
            }

            // Consume expressions
            let (exprs, token, end_span, comment) = (|| {
                let mut exprs = Vec::new();
                let mut comment = None;
                loop {
                    let (span, token) = self.peek();

                    match token {
                        Some(Token::LineBreak) => {
                            self.consume();
                            return (exprs, token, None, comment);
                        }
                        Some(Token::Comment(c)) => {
                            self.consume();
                            comment = Some(InternedString::new(c))
                        }
                        Some(token) if token == end_token => {
                            self.consume();
                            return (exprs, Some(token), Some(span), comment);
                        }
                        None => {
                            self.consume();
                            return (exprs, token, Some(span), comment);
                        }
                        _ => {
                            if let Some(expr) = self.parse_expr() {
                                exprs.push(expr);
                            } else {
                                error = true;
                                return (exprs, token, None, comment);
                            }
                        }
                    }
                }
            })();

            lines.push(ListLine { exprs, comment });

            if let Some(end_span) = end_span {
                break (token, end_span);
            }
        };

        if parsed_end_token.is_none() {
            self.diagnostics.add(Diagnostic::new(
                DiagnosticLevel::Error,
                "syntax error",
                vec![Note::primary(
                    self.eof_span(),
                    format!("expected {}, found end of file", end_token),
                )],
            ));

            return None;
        }

        (!error).then(|| (lines, end_span))
    }

    pub fn try_parse_block(&mut self) -> Result<Expr, ParseError> {
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

    pub fn peek(&mut self) -> (Span, Option<Token<'src>>) {
        match self.lexer.peek().cloned() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    pub fn consume(&mut self) -> (Span, Option<Token<'src>>) {
        match self.lexer.next() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    pub fn offset(&self, range: Range<usize>) -> Span {
        Span::new(
            self.file,
            (range.start + self.offset)..(range.end + self.offset),
        )
    }

    pub fn eof_span(&self) -> Span {
        self.offset(self.len..self.len)
    }

    pub fn file_span(&self) -> Span {
        self.offset(0..self.len)
    }
}

pub fn parse_shebang(code: &str) -> Option<(&str, &str)> {
    lazy_static! {
        static ref SHEBANG_REGEX: Regex = Regex::new(r#"^#!(.*)"#).unwrap();
    }

    SHEBANG_REGEX
        .find(code)
        .map(|m| (&m.as_str()[2..], &code[m.end()..]))
}
