use crate::{
    diagnostics::*,
    parse::{Span, Token},
    Compiler, FilePath, InternedString,
};
use lazy_static::lazy_static;
use logos::SpannedIter;
use regex::Regex;
use std::{iter::Peekable, ops::Range};

#[derive(Debug, Clone)]
pub struct File {
    pub path: FilePath,
    pub span: Span,
    pub shebang: Option<InternedString>,
    pub attributes: Vec<Expr>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Underscore,
    Name(InternedString),
    Text(InternedString),
    Number(InternedString),
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
    pub attributes: Vec<Expr>,
    pub exprs: Vec<Expr>,
    pub comment: Option<InternedString>,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Expr { span, kind }
    }
}

pub(crate) struct Parser<'a, 'src> {
    pub compiler: &'a Compiler<'a>,
    pub lexer: Peekable<SpannedIter<'src, Token<'src>>>,
    pub len: usize,
    pub offset: usize,
    pub file: FilePath,
}

#[derive(Clone, Copy)]
pub enum ParseError {
    WrongTokenType,
    InvalidExpr,
    EndOfFile,
}

impl<'a, 'src> Parser<'a, 'src> {
    pub fn parse_file(&mut self) -> (Vec<Expr>, Vec<Statement>) {
        while let (_, Some(Token::Comment(_) | Token::LineBreak)) = self.peek() {
            self.consume();
        }

        let attributes = self.parse_file_attributes();

        let (statements, _) = self.parse_statements(None);

        if let (span, Some(token)) = self.consume() {
            self.compiler.add_error(
                "syntax error",
                vec![Note::primary(
                    span,
                    format!("expected end of file, found {}", token),
                )],
            );
        }

        (attributes, statements)
    }

    pub fn parse_file_attributes(&mut self) -> Vec<Expr> {
        std::iter::from_fn(|| self.try_parse_file_attribute()).collect()
    }

    pub fn try_parse_file_attribute(&mut self) -> Option<Expr> {
        let (span, token) = self.peek();

        if !matches!(token, Some(Token::LeftFileBracket)) {
            return None;
        }

        self.consume();

        let (lines, end_span) = self.parse_list_contents(Token::RightFileBracket);

        Some(Expr::new(
            span.with_end(end_span.end),
            ExprKind::List(lines),
        ))
    }

    pub fn parse_statements(&mut self, end_token: Option<Token<'src>>) -> (Vec<Statement>, Span) {
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
            let (attributes, exprs, parsed_end_token, end_span, comment) = (|| {
                let mut attributes = Vec::new();
                let mut exprs = Vec::new();
                let mut comment = None;
                loop {
                    let (span, token) = self.peek();

                    match (token, end_token) {
                        (Some(Token::LineBreak), _) => {
                            self.consume();
                            return (attributes, exprs, token, None, comment);
                        }
                        (Some(Token::Comment(c)), _) => {
                            self.consume();
                            comment = Some(InternedString::new(c))
                        }
                        (None, _) => {
                            self.consume();
                            return (attributes, exprs, token, Some(span), comment);
                        }
                        (Some(token), Some(end_token)) if token == end_token => {
                            self.consume();
                            return (attributes, exprs, Some(token), Some(span), comment);
                        }
                        _ => {
                            if let Some(attribute) = self.try_parse_attribute() {
                                attributes.push(attribute);

                                while let (_, Some(Token::LineBreak)) = self.peek() {
                                    self.consume();
                                }
                            } else if let Some(expr) = self.parse_expr() {
                                exprs.push(expr);
                            } else {
                                error = true;
                                return (attributes, exprs, token, None, comment);
                            }
                        }
                    }
                }
            })();

            if let Some(end_token) = end_token {
                if parsed_end_token.is_none() {
                    self.compiler.add_error(
                        "syntax error",
                        vec![Note::primary(
                            self.eof_span(),
                            format!("expected {}, found end of file", end_token),
                        )],
                    );
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

            statements.last_mut().unwrap().lines.push(ListLine {
                attributes,
                exprs,
                comment,
            });

            if let Some(end_span) = end_span {
                break end_span;
            }
        };

        (statements, end_span)
    }

    pub fn try_parse_attribute(&mut self) -> Option<Expr> {
        let (span, token) = self.peek();

        if !matches!(token, Some(Token::LeftAttrBracket)) {
            return None;
        }

        self.consume();

        let (lines, end_span) = self.parse_list_contents(Token::RightAttrBracket);

        Some(Expr::new(
            span.with_end(end_span.end),
            ExprKind::List(lines),
        ))
    }

    pub fn parse_expr(&mut self) -> Option<Expr> {
        macro_rules! parse_each {
            ($parse:ident $(, $rest:ident)* $(,)?) => {
                match self.$parse() {
                    Ok(expr) => Some(expr),
                    Err(ParseError::WrongTokenType) => parse_each!($($rest),*),
                    Err(ParseError::InvalidExpr) => None,
                    Err(ParseError::EndOfFile) => {
                        self.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(self.eof_span(), "expected expression, found end of file")],
                        );

                        None
                    }
                }
            };
            () => {{
                let (span, token) = self.consume();

                self.compiler.add_error(
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
                );

                None
            }};
        }

        parse_each!(
            try_parse_underscore,
            try_parse_name,
            try_parse_text,
            try_parse_number,
            try_parse_list,
            try_parse_block,
        )
    }

    pub fn try_parse_underscore(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Underscore) => {
                self.consume();

                Ok(Expr::new(span, ExprKind::Underscore))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
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

                        self.compiler.add_error(
                            "syntax error",
                            vec![Note::primary(
                                Span::new(self.file, range).offset(span.start),
                                format!("invalid character sequence in string literal ({:?})", e),
                            )],
                        );
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
            Some(Token::Number(number)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::Number(InternedString::new(number)),
                ))
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

                let (lines, end_span) = self.parse_list_contents(Token::RightParenthesis);

                Ok(Expr::new(
                    span.with_end(end_span.end),
                    ExprKind::List(lines),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn parse_list_contents(&mut self, end_token: Token) -> (Vec<ListLine>, Span) {
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

            lines.push(ListLine {
                attributes: Vec::new(),
                exprs,
                comment,
            });

            if let Some(end_span) = end_span {
                break (token, end_span);
            }
        };

        if parsed_end_token.is_none() {
            self.compiler.add_error(
                "syntax error",
                vec![Note::primary(
                    self.eof_span(),
                    format!("expected {}, found end of file", end_token),
                )],
            );
        }

        (lines, end_span)
    }

    pub fn try_parse_block(&mut self) -> Result<Expr, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftBrace) => {
                self.consume();

                let (statements, end_range) = self.parse_statements(Some(Token::RightBrace));

                Ok(Expr::new(
                    span.with_end(end_range.end),
                    ExprKind::Block(statements),
                ))
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
