use crate::{parse::Token, Driver, Span};
use lazy_static::lazy_static;
use logos::SpannedIter;
use regex::Regex;
use std::{cell::RefCell, iter::Peekable, ops::Range};

#[derive(Debug, Clone)]
pub struct File<D: Driver> {
    pub span: D::Span,
    pub shebang: Option<D::InternedString>,
    pub comments: Vec<ListLine<D>>,
    pub attributes: Vec<Attribute<D>>,
    pub statements: Vec<Statement<D>>,
}

#[derive(Debug, Clone)]
pub struct Attribute<D: Driver> {
    pub span: D::Span,
    pub exprs: Vec<Expr<D>>,
    pub comment: Option<D::InternedString>,
}

#[derive(Debug, Clone)]
pub struct Expr<D: Driver> {
    pub span: D::Span,
    pub kind: ExprKind<D>,
}

#[derive(Debug, Clone)]
pub enum ExprKind<D: Driver> {
    Placeholder(D::InternedString),
    Underscore,
    Name(D::InternedString, Option<D::Scope>),
    QuoteName(D::InternedString),
    RepeatName(D::InternedString),
    Text(D::InternedString, D::InternedString),
    Number(D::InternedString),
    Asset(D::InternedString),
    List(Vec<ListLine<D>>),
    RepeatList(Vec<ListLine<D>>),
    Block(Vec<Statement<D>>, Option<D::Scope>),
    SourceCode(String),
}

impl<D: Driver> Expr<D> {
    pub fn list(span: D::Span, exprs: Vec<Expr<D>>) -> Self {
        Expr {
            span,
            kind: ExprKind::List(vec![exprs.into()]),
        }
    }

    pub fn list_or_expr(span: D::Span, mut exprs: Vec<Expr<D>>) -> Self {
        if exprs.len() == 1 {
            exprs.pop().unwrap()
        } else {
            Expr::list(span, exprs)
        }
    }

    pub fn try_as_list_exprs(
        &self,
    ) -> Result<(D::Span, impl Iterator<Item = &Expr<D>> + Send), &Self> {
        match &self.kind {
            ExprKind::List(lines) => Ok((self.span, lines.iter().flat_map(|line| &line.exprs))),
            _ => Err(self),
        }
    }

    pub fn try_as_list_repetition_exprs(
        &self,
    ) -> Result<(D::Span, impl Iterator<Item = &Expr<D>> + Send), &Self> {
        match &self.kind {
            ExprKind::RepeatList(lines) => {
                Ok((self.span, lines.iter().flat_map(|line| &line.exprs)))
            }
            _ => Err(self),
        }
    }

    pub fn try_into_list_exprs(
        self,
    ) -> Result<(D::Span, impl Iterator<Item = Expr<D>> + Send), Self> {
        match self.kind {
            ExprKind::List(lines) => Ok((self.span, lines.into_iter().flat_map(|line| line.exprs))),
            _ => Err(self),
        }
    }

    pub fn try_into_list_repetition_exprs(
        self,
    ) -> Result<(D::Span, impl Iterator<Item = Expr<D>> + Send), Self> {
        match self.kind {
            ExprKind::RepeatList(lines) => {
                Ok((self.span, lines.into_iter().flat_map(|line| line.exprs)))
            }
            _ => Err(self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Statement<D: Driver> {
    pub lines: Vec<ListLine<D>>,
}

#[derive(Debug, Clone)]
pub struct ListLine<D: Driver> {
    pub leading_lines: u32,
    pub attributes: Vec<Attribute<D>>,
    pub exprs: Vec<Expr<D>>,
    pub comment: Option<D::InternedString>,
}

impl<D: Driver> From<Vec<Expr<D>>> for ListLine<D> {
    fn from(exprs: Vec<Expr<D>>) -> Self {
        ListLine {
            leading_lines: 0,
            attributes: Vec::new(),
            exprs,
            comment: None,
        }
    }
}

impl<D: Driver> Expr<D> {
    pub fn new(span: D::Span, kind: ExprKind<D>) -> Self {
        Expr { span, kind }
    }
}

impl<D: Driver> Expr<D> {
    pub fn fix_to(&mut self, scope: D::Scope, make_child: impl Fn(D::Scope) -> D::Scope) {
        let scopes = RefCell::new(vec![scope]);

        self.traverse_mut(
            |expr| match &mut expr.kind {
                ExprKind::Name(_, name_scope) => {
                    name_scope.get_or_insert_with(|| *scopes.borrow().last().unwrap());
                }
                ExprKind::Block(_, block_scope) => {
                    let scope =
                        block_scope.unwrap_or_else(|| make_child(*scopes.borrow().last().unwrap()));

                    scopes.borrow_mut().push(scope);

                    *block_scope = Some(scope);
                }
                _ => {}
            },
            |expr| {
                if let ExprKind::Block(_, _) = &mut expr.kind {
                    scopes.borrow_mut().pop().unwrap();
                }
            },
        )
    }

    pub fn traverse_mut(
        &mut self,
        mut enter: impl FnMut(&mut Self),
        mut leave: impl FnMut(&mut Self),
    ) {
        self.traverse_mut_inner(&mut enter, &mut leave);
    }

    fn traverse_mut_inner(
        &mut self,
        enter: &mut impl FnMut(&mut Self),
        leave: &mut impl FnMut(&mut Self),
    ) {
        enter(self);

        match &mut self.kind {
            ExprKind::List(lines) | ExprKind::RepeatList(lines) => {
                for line in lines {
                    for attribute in &mut line.attributes {
                        for expr in &mut attribute.exprs {
                            expr.traverse_mut_inner(enter, leave);
                        }
                    }

                    for expr in &mut line.exprs {
                        expr.traverse_mut_inner(enter, leave);
                    }
                }
            }
            ExprKind::Block(statements, _) => {
                for statement in statements {
                    for line in &mut statement.lines {
                        for attribute in &mut line.attributes {
                            for expr in &mut attribute.exprs {
                                expr.traverse_mut_inner(enter, leave);
                            }
                        }

                        for expr in &mut line.exprs {
                            expr.traverse_mut_inner(enter, leave);
                        }
                    }
                }
            }
            _ => {}
        }

        leave(self);
    }
}

pub(crate) struct Parser<'src, 'a, D: Driver> {
    pub driver: &'a D,
    pub path: D::Path,
    pub lexer: Peekable<SpannedIter<'src, Token<'src>>>,
    pub len: usize,
    pub offset: usize,
}

#[derive(Clone, Copy)]
pub enum ParseError {
    WrongTokenType,
    InvalidExpr,
    EndOfFile,
}

pub struct FileContents<D: Driver> {
    pub comments: Vec<ListLine<D>>,
    pub attributes: Vec<Attribute<D>>,
    pub statements: Vec<Statement<D>>,
}

impl<'src, 'a, D: Driver> Parser<'src, 'a, D> {
    pub fn parse_file(&mut self) -> FileContents<D> {
        let mut comments = Vec::new();
        loop {
            let mut leading_lines = 0;
            while let (_, Some(Token::LineBreak)) = self.peek() {
                leading_lines += 1;
                self.consume();
            }

            if let (_, Some(Token::Comment(c))) = self.peek() {
                comments.push(ListLine {
                    leading_lines,
                    attributes: Vec::new(),
                    exprs: Vec::new(),
                    comment: Some(self.driver.intern(c)),
                });

                self.consume();
            } else {
                break;
            }
        }

        let attributes = self.parse_file_attributes();

        let (statements, _) = self.parse_statements(None);

        if let (span, Some(token)) = self.consume() {
            self.driver
                .syntax_error(span, format!("expected end of file, found {token}"));
        }

        FileContents {
            comments,
            attributes,
            statements,
        }
    }

    pub fn parse_file_attributes(&mut self) -> Vec<Attribute<D>> {
        std::iter::from_fn(|| self.try_parse_file_attribute()).collect()
    }

    pub fn try_parse_file_attribute(&mut self) -> Option<Attribute<D>> {
        while let (_, Some(Token::LineBreak)) = self.peek() {
            self.consume();
        }

        let (span, token) = self.peek();

        if !matches!(token, Some(Token::LeftFileBracket)) {
            return None;
        }

        self.consume();

        if let (_, Some(Token::LineBreak)) = self.peek() {
            self.consume();
        }

        let (lines, end_span) = self.parse_list_contents(Token::RightFileBracket);

        let mut comment = None;
        if let (_, Some(Token::Comment(c))) = self.peek() {
            comment = Some(self.driver.intern(c));
            self.consume();
        }

        Some(Attribute {
            span: Span::join(span, end_span),
            exprs: lines.into_iter().flat_map(|line| line.exprs).collect(),
            comment,
        })
    }

    pub fn parse_statements(
        &mut self,
        end_token: Option<Token<'src>>,
    ) -> (Vec<Statement<D>>, D::Span) {
        let mut statements = Vec::<Statement<D>>::new();
        let mut error = false;

        let end_span = loop {
            // Consume line breaks
            let mut leading_lines = 0;
            while let (_, Some(Token::LineBreak)) = self.peek() {
                leading_lines += 1;
                self.consume();
            }

            // Consume expressions
            let (lines, parsed_end_token, end_span) = (|| {
                let mut lines = vec![ListLine {
                    leading_lines,
                    attributes: Vec::new(),
                    exprs: Vec::new(),
                    comment: None,
                }];

                loop {
                    let (span, token) = self.peek();

                    match (token, end_token) {
                        (Some(Token::LineBreak), _) => {
                            self.consume();
                            return (lines, token, None);
                        }
                        (Some(Token::LineContinue), _) => {
                            self.consume();

                            lines.push(ListLine {
                                leading_lines: 0,
                                attributes: Vec::new(),
                                exprs: Vec::new(),
                                comment: None,
                            });
                        }
                        (Some(Token::Comment(c)), _) => {
                            self.consume();
                            lines.last_mut().unwrap().comment = Some(self.driver.intern(c));
                        }
                        (None, _) => {
                            self.consume();
                            return (lines, token, Some(span));
                        }
                        (Some(token), Some(end_token)) if token == end_token => {
                            self.consume();
                            return (lines, Some(token), Some(span));
                        }
                        _ => {
                            if let Some(attribute) = self.try_parse_attribute() {
                                lines.last_mut().unwrap().attributes.push(attribute);

                                while let (_, Some(Token::LineBreak)) = self.peek() {
                                    self.consume();
                                }
                            } else if let Some(expr) = self.parse_expr() {
                                lines.last_mut().unwrap().exprs.push(expr);
                            } else {
                                error = true;
                                return (lines, token, None);
                            }
                        }
                    }
                }
            })();

            if let Some(end_token) = end_token {
                if parsed_end_token.is_none() {
                    let span = self.eof_span();

                    self.driver
                        .syntax_error(span, format!("expected {end_token}, found end of file"));
                }
            }

            let first_line = lines.first().unwrap();

            if first_line.leading_lines > 0
                || !first_line.attributes.is_empty()
                || !first_line.exprs.is_empty()
                || first_line.comment.is_some()
            {
                statements.push(Statement { lines });
            }

            if let Some(end_span) = end_span {
                break end_span;
            }
        };

        (statements, end_span)
    }

    pub fn try_parse_attribute(&mut self) -> Option<Attribute<D>> {
        let (span, token) = self.peek();

        if !matches!(token, Some(Token::LeftAttrBracket)) {
            return None;
        }

        self.consume();

        if let (_, Some(Token::LineBreak)) = self.peek() {
            self.consume();
        }

        let (lines, end_span) = self.parse_list_contents(Token::RightAttrBracket);

        let mut comment = None;
        if let (_, Some(Token::Comment(c))) = self.peek() {
            comment = Some(self.driver.intern(c));
            self.consume();
        }

        Some(Attribute {
            span: Span::join(span, end_span),
            exprs: lines.into_iter().flat_map(|line| line.exprs).collect(),
            comment,
        })
    }

    pub fn parse_expr(&mut self) -> Option<Expr<D>> {
        macro_rules! parse_each {
            ($parse:ident $(, $rest:ident)* $(,)?) => {
                match self.$parse() {
                    Ok(expr) => Some(expr),
                    Err(ParseError::WrongTokenType) => parse_each!($($rest),*),
                    Err(ParseError::InvalidExpr) => None,
                    Err(ParseError::EndOfFile) => {
                        let span = self.eof_span();

                        self.driver
                            .syntax_error(span, "expected expression, found end of file");

                        None
                    }
                }
            };
            () => {{
                let (span, token) = self.consume();

                self.driver.syntax_error(
                    span,
                    format!(
                        "expected expression, found {}",
                        token
                            .map(|t| t.to_string())
                            .as_deref()
                            .unwrap_or("end of file")
                    ),
                );

                None
            }};
        }

        parse_each!(
            try_parse_placeholder,
            try_parse_underscore,
            try_parse_name,
            try_parse_quote_name,
            try_parse_repeat_name,
            try_parse_text,
            try_parse_number,
            try_parse_asset,
            try_parse_list,
            try_parse_repeat_list,
            try_parse_block,
        )
    }

    pub fn try_parse_placeholder(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Placeholder(placeholder)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::Placeholder(self.driver.intern(placeholder)),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_underscore(&mut self) -> Result<Expr<D>, ParseError> {
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

    pub fn try_parse_name(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Name(name)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::Name(self.driver.intern(name), None),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_quote_name(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::QuoteName(name)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::QuoteName(self.driver.intern(name)),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_repeat_name(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::RepeatName(name)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::RepeatName(self.driver.intern(name)),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_text(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Text(raw)) => {
                self.consume();

                let mut unescaped = String::with_capacity(raw.len());
                let mut error = false;

                rustc_lexer::unescape::unescape_str(raw, &mut |range, result| match result {
                    Ok(ch) => unescaped.push(ch),
                    Err(e) => {
                        error = true;

                        let span = self.offset(
                            (range.start + span.range().start)..(range.end + span.range().end),
                        );

                        self.driver.syntax_error(
                            span,
                            format!("invalid character sequence in string literal ({e:?})"),
                        )
                    }
                });

                if error {
                    Err(ParseError::InvalidExpr)
                } else {
                    Ok(Expr::new(
                        span,
                        ExprKind::Text(self.driver.intern(unescaped), self.driver.intern(raw)),
                    ))
                }
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_asset(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Asset(raw)) => {
                self.consume();

                Ok(Expr::new(span, ExprKind::Asset(self.driver.intern(raw))))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_number(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::Number(number)) => {
                self.consume();

                Ok(Expr::new(
                    span,
                    ExprKind::Number(self.driver.intern(number)),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_list(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftParenthesis) => {
                self.consume();

                if let (_, Some(Token::LineBreak)) = self.peek() {
                    self.consume();
                }

                let (lines, end_span) = self.parse_list_contents(Token::RightParenthesis);

                Ok(Expr::new(Span::join(span, end_span), ExprKind::List(lines)))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn try_parse_repeat_list(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::RepeatLeftParenthesis) => {
                self.consume();

                if let (_, Some(Token::LineBreak)) = self.peek() {
                    self.consume();
                }

                let (lines, end_span) = self.parse_list_contents(Token::RightParenthesis);

                Ok(Expr::new(
                    Span::join(span, end_span),
                    ExprKind::RepeatList(lines),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn parse_list_contents(&mut self, end_token: Token) -> (Vec<ListLine<D>>, D::Span) {
        let mut lines = Vec::<ListLine<D>>::new();
        let mut error = false;

        let (parsed_end_token, end_span) = loop {
            // Consume line breaks
            let mut leading_lines = 0;
            while let (_, Some(Token::LineBreak)) = self.peek() {
                leading_lines += 1;
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
                            comment = Some(self.driver.intern(c));
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

            if leading_lines > 0 || !exprs.is_empty() || comment.is_some() {
                lines.push(ListLine {
                    leading_lines,
                    attributes: Vec::new(),
                    exprs,
                    comment,
                });
            }

            if let Some(end_span) = end_span {
                break (token, end_span);
            }
        };

        if parsed_end_token.is_none() {
            let span = self.eof_span();

            self.driver
                .syntax_error(span, format!("expected {end_token}, found end of file"));
        }

        (lines, end_span)
    }

    pub fn try_parse_block(&mut self) -> Result<Expr<D>, ParseError> {
        let (span, token) = self.peek();

        match token {
            Some(Token::LeftBrace) => {
                self.consume();

                if let (_, Some(Token::LineBreak)) = self.peek() {
                    self.consume();
                }

                let (statements, end_span) = self.parse_statements(Some(Token::RightBrace));

                Ok(Expr::new(
                    Span::join(span, end_span),
                    ExprKind::Block(statements, None),
                ))
            }
            Some(_) => Err(ParseError::WrongTokenType),
            None => Err(ParseError::EndOfFile),
        }
    }

    pub fn peek(&mut self) -> (D::Span, Option<Token<'src>>) {
        match self.lexer.peek().cloned() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    pub fn consume(&mut self) -> (D::Span, Option<Token<'src>>) {
        match self.lexer.next() {
            Some((token, span)) => (self.offset(span), Some(token)),
            None => (self.eof_span(), None),
        }
    }

    pub fn offset(&mut self, range: Range<usize>) -> D::Span {
        self.driver.make_span(
            self.path,
            (range.start + self.offset)..(range.end + self.offset),
        )
    }

    pub fn eof_span(&mut self) -> D::Span {
        self.offset(self.len..self.len)
    }

    pub fn file_span(&mut self) -> D::Span {
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
