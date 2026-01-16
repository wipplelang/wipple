use crate::{
    database::{Db, Node, NodeRef, Span},
    syntax::{ParseError, Token, TokenKind, tokenize},
};
use std::{
    any::{Any, TypeId},
    collections::HashMap,
};

#[derive(Debug)]
pub struct Parser<'db> {
    db: &'db mut Db,
    source: String,
    tokens: Vec<Token>,
    index: usize,
    stack: Vec<StackEntry>,
    cache: Cache,
}

#[derive(Debug, Default)]
struct StackEntry {
    trace: Option<String>,
}

#[derive(Debug)]
struct CacheEntry {
    index: usize,
    value: Box<dyn Any>,
}

#[derive(Debug, Default)]
struct Cache(HashMap<TypeId, HashMap<usize, CacheEntry>>);

impl<'db> Parser<'db> {
    pub fn new(db: &'db mut Db, path: &str, source: &str) -> Result<Self, ParseError> {
        let tokens = tokenize(path, source)?;

        Ok(Self {
            db,
            source: source.to_string(),
            tokens,
            index: 0,
            stack: Vec::new(),
            cache: Cache(HashMap::new()),
        })
    }

    pub fn spanned(&self) -> impl FnOnce(&mut Self) -> Span + use<'db> {
        let start = self
            .tokens
            .get(self.index)
            .map_or_else(|| self.eof_span(), |token| token.span.clone());

        move |parser| {
            let end = parser
                .tokens
                .get(parser.index.saturating_sub(1))
                .map_or_else(|| parser.eof_span(), |token| token.span.clone());

            Span::join(start, end, &parser.source)
        }
    }

    pub fn backtrack(&mut self, index: usize) {
        self.index = index;
    }

    pub fn error(&self, message: impl Into<String>) -> ParseError {
        self.error_with_reason(message, None)
    }

    pub fn error_with_reason(
        &self,
        message: impl Into<String>,
        reason: Option<String>,
    ) -> ParseError {
        let span = self
            .tokens
            .get(self.index)
            .map_or_else(|| self.eof_span(), |token| token.span.clone());

        ParseError {
            message: message.into(),
            reason,
            committed: None,
            span,
        }
    }

    pub fn token(&mut self, kind: TokenKind) -> Result<String, ParseError> {
        self.token_with(kind, None, None)
    }

    pub fn token_with_name(&mut self, kind: TokenKind, name: &str) -> Result<String, ParseError> {
        self.token_with(kind, Some(name), None)
    }

    pub fn token_with_reason(
        &mut self,
        kind: TokenKind,
        reason: &str,
    ) -> Result<String, ParseError> {
        self.token_with(kind, None, Some(reason))
    }

    pub fn token_with(
        &mut self,
        kind: TokenKind,
        name: Option<&str>,
        reason: Option<&str>,
    ) -> Result<String, ParseError> {
        let expected = name.unwrap_or_else(|| kind.name());

        let Some(token) = &self.tokens.get(self.index) else {
            return Err(ParseError {
                message: format!("Expected {}", expected),
                reason: reason.map(|s| s.to_string()),
                committed: None,
                span: self.eof_span(),
            });
        };

        if token.kind != kind {
            return Err(ParseError {
                message: format!("Expected {}, but found {}", expected, token.kind.name()),
                reason: reason.map(|s| s.to_string()),
                committed: None,
                span: token.span.clone(),
            });
        }

        self.index += 1;

        Ok(token.value.clone())
    }

    pub fn try_node<T: Node>(
        &mut self,
        f: impl FnOnce(&mut Parser<'_>) -> Result<T, ParseError>,
    ) -> Result<Option<NodeRef>, ParseError> {
        self.parse_optional(|parser| parser.node(f))
    }

    pub fn node<T: Node>(
        &mut self,
        f: impl FnOnce(&mut Parser<'_>) -> Result<T, ParseError>,
    ) -> Result<NodeRef, ParseError> {
        let span = self.spanned();
        let node = f(self)?;
        let span = span(self);
        Ok(self.register(span, node))
    }

    pub fn register<T: Node>(&mut self, span: Span, node: T) -> NodeRef {
        self.db.node(span, node)
    }

    pub fn span(&self, node: &NodeRef) -> Span {
        self.db.span(node)
    }

    pub fn join_spans(&self, left: &Span, right: &Span) -> Span {
        Span::join(left.clone(), right.clone(), &self.source)
    }

    pub fn commit(&mut self, trace: &str) {
        self.stack.last_mut().unwrap().trace = Some(trace.to_string());
    }

    pub fn consume_line_breaks(&mut self) {
        let _ = self.token(TokenKind::LineBreak);
    }

    fn eof_span(&self) -> Span {
        self.tokens
            .last()
            .map_or_else(Span::empty, |token| token.span.clone())
    }

    pub fn finish(self) -> Result<(), ParseError> {
        if let Some(token) = self.tokens.get(self.index) {
            return Err(self.error(format!("Unexpected {}", token.kind.name())));
        }

        Ok(())
    }

    pub fn parse_nothing(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    pub fn parse_cached<
        T: Clone + 'static,
        F: Fn(&mut Parser<'_>) -> Result<T, ParseError> + 'static,
    >(
        &mut self,
        f: F,
    ) -> Result<T, ParseError> {
        let key = TypeId::of::<F>();
        let start = self.index;

        if let Some(cached) = self.cache.0.entry(key).or_default().get(&start) {
            let value = cached.value.downcast_ref::<T>().unwrap();
            self.index = cached.index;
            return Ok(value.clone());
        }

        let result = f(self)?;

        self.cache.0.entry(key).or_default().insert(
            start,
            CacheEntry {
                index: self.index,
                value: Box::new(result.clone()),
            },
        );

        Ok(result)
    }

    pub fn parse_optional<T>(
        &mut self,
        f: impl FnOnce(&mut Parser<'_>) -> Result<T, ParseError>,
    ) -> Result<Option<T>, ParseError> {
        let start = self.index;

        let entry = StackEntry::default();
        self.stack.push(entry);
        let result = f(self);
        let entry = self.stack.pop().unwrap();

        match result {
            Ok(value) => Ok(Some(value)),
            Err(mut error) => {
                if let Some(trace) = entry.trace {
                    error.committed = Some(trace);
                }

                if error.committed.is_some() {
                    return Err(error);
                }

                self.backtrack(start);

                Ok(None)
            }
        }
    }

    pub fn parse_many<T, S: Default>(
        &mut self,
        min: usize,
        f: impl Fn(&mut Parser<'_>) -> Result<T, ParseError>,
        separator: impl Fn(&mut Parser<'_>) -> Result<S, ParseError>,
    ) -> Result<Vec<(T, (S, Span))>, ParseError> {
        let mut first = true;

        let mut results = Vec::new();
        loop {
            let start = self.index;

            let span = self.spanned();

            let mut separator_result = S::default();
            if !first {
                match self.parse_optional(&separator)? {
                    Some(result) => separator_result = result,
                    None => break,
                }
            }

            let separator_span = span(self);

            let result = match self.parse_optional(&f)? {
                Some(result) => result,
                None => {
                    self.backtrack(start);
                    break;
                }
            };

            results.push((result, (separator_result, separator_span)));

            first = false;
        }

        if results.len() < min {
            let span = match results.last() {
                Some((_, (_, span))) => span.clone(),
                None => self.eof_span(),
            };

            return Err(ParseError {
                message: format!("Expected at least {} items", min),
                reason: None,
                committed: None,
                span,
            });
        }

        Ok(results)
    }

    pub fn parse_lines<T, F>(
        &mut self,
        min: usize,
        require: bool,
        f: F,
    ) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser<'_>) -> Result<T, ParseError>,
    {
        self.consume_line_breaks();

        let result = self.parse_many(min, f, |parser| {
            if require {
                parser.token(TokenKind::LineBreak)?;
            } else {
                parser.consume_line_breaks();
            }

            Ok(())
        })?;

        self.consume_line_breaks();

        Ok(result.into_iter().map(|(result, _)| result).collect())
    }
}
