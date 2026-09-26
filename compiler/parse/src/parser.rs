use serde::{Deserialize, Serialize};
use std::{
    any::TypeId,
    collections::{BTreeMap, HashMap},
};
use wipple_core::{
    ast::AstKey,
    db::{Db, Fact},
    render::{Render, RenderCtx},
    span::{Span, Str},
    visit::Visit,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseError {
    pub message: String,
    pub reason: Option<String>,
    pub committed: Option<String>,
    pub span: Span,
}

#[typetag::serde]
impl Fact for ParseError {}

impl Render for ParseError {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string(&self.message);

        if let Some(committed) = &self.committed {
            ctx.string(format!(" {committed}"));
        }
    }
}

#[derive(Default)]
struct StackEntry {
    trace: Option<&'static str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CacheToken(pub TypeId);

#[macro_export]
macro_rules! cache_token {
    () => {{
        struct T;
        $crate::parser::CacheToken(std::any::TypeId::of::<T>())
    }};
}

use crate::lexer::{Token, TokenKind, tokenize};
pub use cache_token;

struct CacheEntry {
    index: usize,
    value: AstKey,
}

pub struct ParseToken {
    pub matching: Box<dyn Fn(TokenKind) -> bool>,
    pub name: &'static str,
    pub expected: Option<String>,
    pub reason: Option<&'static str>,
}

impl From<TokenKind> for ParseToken {
    fn from(token: TokenKind) -> Self {
        ParseToken::new(move |kind| kind == token, token.name())
    }
}

impl ParseToken {
    pub fn new(matching: impl Fn(TokenKind) -> bool + 'static, name: &'static str) -> Self {
        ParseToken {
            matching: Box::new(matching),
            name,
            expected: None,
            reason: None,
        }
    }

    pub fn name(mut self, name: &'static str) -> Self {
        self.name = name;
        self
    }

    pub fn expected(mut self, expected: impl Into<String>) -> Self {
        self.expected = Some(expected.into());
        self
    }

    pub fn reason(mut self, reason: &'static str) -> Self {
        self.reason = Some(reason);
        self
    }
}

pub struct Parser<'a> {
    pub db: &'a mut Db,
    pub source: Str,
    tokens: Vec<Token>,
    index: usize,
    stack: Vec<StackEntry>,
    cache: HashMap<CacheToken, BTreeMap<usize, CacheEntry>>,
}

impl<'a> Parser<'a> {
    pub fn new(
        db: &'a mut Db,
        path: impl Into<Str>,
        source: impl Into<Str>,
    ) -> Result<Self, ParseError> {
        let source = source.into();
        let tokens = tokenize(path, source.clone())?;

        Ok(Parser {
            db,
            source,
            tokens,
            index: 0,
            stack: Default::default(),
            cache: Default::default(),
        })
    }

    pub fn spanned(&self) -> impl Fn(&Self) -> Span + use<'a> {
        let start = self.span_at(self.index);

        move |parser| {
            let end = parser.span_at(parser.index.saturating_sub(1));
            start.join_in(&end, parser.source.clone())
        }
    }

    pub fn in_ast(&mut self, value: impl Visit) -> AstKey {
        self.db.in_ast(Box::new(value))
    }

    pub fn error(&self, message: impl Into<String>, reason: Option<String>) -> ParseError {
        let span = self.span_at(self.index);

        ParseError {
            message: message.into(),
            reason,
            committed: None,
            span,
        }
    }

    pub fn token(&mut self, parse: impl Into<ParseToken>) -> Result<Str, ParseError> {
        let parse = parse.into();

        let expected = parse.expected.unwrap_or_else(|| parse.name.to_string());

        if self.index >= self.tokens.len() {
            return Err(self.error(
                format!("Expected {expected}"),
                parse.reason.map(String::from),
            ));
        }

        let token = &self.tokens[self.index];

        if !(parse.matching)(token.kind) {
            return Err(self.error(
                format!("Expected {expected}, but found {}", token.kind.name()),
                parse.reason.map(String::from),
            ));
        }

        self.index += 1;

        Ok(token.value.clone())
    }

    pub fn soft_keyword(&mut self, keyword: &'static str) -> Result<bool, ParseError> {
        self.parse_optional(|parser| {
            let token = parser.token(TokenKind::LowercaseName)?;
            if token != keyword {
                return Err(parser.error(format!("Expected `{keyword}`"), None));
            }

            Ok(())
        })
        .map(|result| result.is_some())
    }

    pub fn commit(&mut self, trace: &'static str) {
        self.stack.last_mut().unwrap().trace = Some(trace);
    }

    pub fn consume_line_breaks(&mut self) {
        self.token(TokenKind::LineBreak).ok();
    }

    pub fn finish(self) -> Result<(), ParseError> {
        if self.index < self.tokens.len() {
            let token = &self.tokens[self.index];

            return Err(self.error(format!("Unexpected {}", token.kind.name()), None));
        }

        Ok(())
    }

    fn backtrack_to(&mut self, index: usize) {
        self.index = index;
    }

    fn span_at(&self, index: usize) -> Span {
        if index >= self.tokens.len() {
            self.eof_span()
        } else {
            self.tokens[index].span.clone()
        }
    }

    fn eof_span(&self) -> Span {
        self.tokens
            .last()
            .map_or_else(Span::empty, |token| token.span.clone())
    }
}

impl Parser<'_> {
    pub fn parse_nothing() -> Result<(), ParseError> {
        Ok(())
    }

    pub fn parse_cached(
        &mut self,
        token: CacheToken,
        parse: impl FnOnce(&mut Self) -> Result<AstKey, ParseError>,
    ) -> Result<AstKey, ParseError> {
        let start = self.index;

        if let Some(cached) = self.cache.get(&token).and_then(|cache| cache.get(&start)) {
            self.index = cached.index;
            return Ok(cached.value.clone());
        }

        let result = parse(self)?;

        self.cache.entry(token).or_default().insert(
            start,
            CacheEntry {
                index: self.index,
                value: result.clone(),
            },
        );

        Ok(result)
    }

    pub fn parse_optional<T>(
        &mut self,
        parse: impl FnOnce(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Option<T>, ParseError> {
        let start = self.index;

        let entry = StackEntry::default();
        self.stack.push(entry);

        let result = parse(self);

        let entry = self.stack.pop().unwrap();

        match result {
            Ok(result) => Ok(Some(result)),
            Err(mut error) => {
                if let Some(trace) = entry.trace {
                    error.committed = Some(String::from(trace));
                }

                if error.committed.is_some() {
                    return Err(error);
                }

                self.backtrack_to(start);

                Ok(None)
            }
        }
    }

    pub fn parse_many<T>(
        &mut self,
        min: usize,
        mut parse: impl FnMut(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Vec<T>, ParseError> {
        let mut last_span = None;
        let mut results = Vec::new();

        loop {
            let start = self.index;

            let span = self.spanned();

            let Some(result) = self.parse_optional(&mut parse)? else {
                self.backtrack_to(start);
                break;
            };

            last_span = Some(span(self));
            results.push(result);
        }

        if results.len() < min {
            let span = last_span.unwrap_or_else(|| self.eof_span());

            return Err(ParseError {
                message: format!("Expected at least {min} items"),
                reason: None,
                committed: None,
                span,
            });
        }

        Ok(results)
    }

    pub fn parse_sep<T, S>(
        &mut self,
        min: usize,
        mut parse: impl FnMut(&mut Self) -> Result<T, ParseError>,
        mut sep: impl FnMut(&mut Self) -> Result<S, ParseError>,
    ) -> Result<Vec<(T, (Option<S>, Span))>, ParseError> {
        let mut first = true;

        let mut results = Vec::new();
        loop {
            let start = self.index;

            let span = self.spanned();

            let mut sep_result = None;
            if !first {
                if let Some(result) = self.parse_optional(&mut sep)? {
                    sep_result = Some(result);
                } else {
                    break;
                }
            }

            let sep_span = span(self);

            let Some(result) = self.parse_optional(&mut parse)? else {
                self.backtrack_to(start);
                break;
            };

            results.push((result, (sep_result, sep_span)));

            first = false;
        }

        if results.len() < min {
            let span = results
                .last()
                .map_or_else(|| self.eof_span(), |(_, (_, span))| span.clone());

            return Err(ParseError {
                message: format!("Expected at least {min} items"),
                reason: None,
                committed: None,
                span,
            });
        }

        Ok(results)
    }

    pub fn parse_lines<T>(
        &mut self,
        min: usize,
        require_line_breaks: bool,
        parse: impl FnMut(&mut Self) -> Result<T, ParseError>,
    ) -> Result<Vec<T>, ParseError> {
        self.consume_line_breaks();

        let result = self.parse_sep(min, parse, |parser| {
            if require_line_breaks {
                parser.token(TokenKind::LineBreak)?;
            } else {
                parser.consume_line_breaks();
            }

            Ok(())
        })?;

        self.consume_line_breaks();

        Ok(result.into_iter().map(|(line, _)| line).collect())
    }
}

#[macro_export]
macro_rules! parse_alt {
    ($parser:ident, {
        $($parse:ident as $x:ident => $result:expr,)*
        _ => $error:literal,
    }) => {{
        static TOKEN: $crate::parser::CacheToken = $crate::parser::cache_token!();

        $parser.parse_cached(TOKEN, |$parser| {
            $(
                if let Some($x) = $parser.parse_optional($parse)? {
                    return Ok($result);
                }
            )*

            Err($parser.error($error, None))
        })
    }};
}

pub use parse_alt;
