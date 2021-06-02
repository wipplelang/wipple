use crate::lexer::{Token, Tokens};
use line_col::LineColLookup;
use std::ops::Range;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub location: Option<SourceLocation>,
}

impl Error {
    fn new(message: &str, offset: Option<&Range<usize>>, lc: &LineColLookup) -> Self {
        Error {
            message: String::from(message),
            location: offset.map(|offset| SourceLocation::new(offset, lc)),
        }
    }
}

#[derive(Debug)]
pub struct Ast {
    pub node: AstNode,
    pub location: SourceLocation,
}

impl Ast {
    fn new(node: AstNode, offset: &Range<usize>, lc: &LineColLookup) -> Self {
        Ast {
            node,
            location: SourceLocation::new(offset, lc),
        }
    }
}

#[derive(Debug)]
pub enum AstNode {
    Block(Vec<AstNodeStatement>),
    List(Vec<Ast>),
    Name(String),
    Text(String),
    Number(String),
    Literal(Box<Ast>),
    Escaped(Box<Ast>),
}

#[derive(Debug)]
pub struct AstNodeStatement {
    pub items: Vec<Ast>,
    pub location: SourceLocation,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

macro_rules! parse_single_token {
    ($token:ident, $tokens:expr) => {
        Ok(match $tokens.peek() {
            Some((Token::$token, _)) => {
                $tokens.next();
                Some(())
            }
            _ => None,
        })
    };

    ($token:ident, $tokens:expr, $lc:expr) => {
        Ok(match $tokens.peek() {
            Some((Token::$token(value), offset)) => {
                $tokens.next();
                Some(Ast::new(AstNode::$token(value.clone()), offset, $lc))
            }
            _ => None,
        })
    };
}

macro_rules! parse_prefix_token {
    ($token:ident => $node:ident, $tokens:expr, $lc:expr) => {{
        let offset = match $tokens.peek() {
            Some((Token::$token, offset)) => {
                $tokens.next();
                offset
            }
            _ => return Ok(None),
        };

        let value =
            parse_value($tokens, $lc)?.ok_or_else(|| Error::new("Expected value", None, $lc))?;

        Ok(Some(Ast::new(AstNode::$node(Box::new(value)), offset, $lc)))
    }};
}

pub fn parse_block(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    let start_offset = match tokens.peek() {
        Some((Token::OpenBrace, offset)) => {
            tokens.next();
            offset
        }
        _ => return Ok(None),
    };

    let statements = parse_statements(tokens, lc)?;

    parse_newlines(tokens)?;

    if let Some((token, end_offset)) = tokens.next() {
        match token {
            Token::CloseBrace => Ok(Some(Ast::new(AstNode::Block(statements), start_offset, lc))),
            _ => Err(Error::new("Expected '}'", Some(end_offset), lc)),
        }
    } else {
        Err(Error::new("Expected '}'", None, lc))
    }
}

pub fn parse_file(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Ast> {
    parse_single_token!(Shebang, tokens)?;

    let statements = parse_statements(tokens, lc)?;

    parse_newlines(tokens)?;

    match tokens.next() {
        None => Ok(Ast::new(AstNode::Block(statements), &(0..0), lc)),
        Some((_, offset)) => Err(Error::new("Unexpected token", Some(offset), lc)),
    }
}

pub fn parse_inline_program(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Ast> {
    parse_single_token!(Shebang, tokens)?;

    let mut values = Vec::new();

    while let Some(value) = parse_value(tokens, lc)? {
        values.push(value);
    }

    match tokens.next() {
        None => Ok(Ast::new(AstNode::List(values), &(0..0), lc)),
        Some((_, offset)) => Err(Error::new("Unexpected token", Some(offset), lc)),
    }
}

fn parse_statements(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Vec<AstNodeStatement>> {
    let mut values: Vec<(Vec<Ast>, Option<SourceLocation>)> = vec![(Vec::new(), None)];

    loop {
        if parse_newline(tokens)?.is_some() {
            values.push((Vec::new(), None));
            continue;
        }

        parse_newlines(tokens)?;

        if let Some(value) = parse_value(tokens, lc)? {
            let (items, location) = values.last_mut().unwrap();
            location.get_or_insert(value.location);
            items.push(value);
        } else {
            break;
        }
    }

    let statements = values
        .into_iter()
        .filter_map(|(items, location)| {
            location.map(|location| AstNodeStatement { items, location })
        })
        .collect::<Vec<_>>();

    Ok(statements)
}

pub fn parse_list(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    let start_offset = match tokens.peek() {
        Some((Token::OpenParenthesis, offset)) => {
            tokens.next();
            offset
        }
        _ => return Ok(None),
    };

    let mut values = Vec::new();

    loop {
        parse_newlines(tokens)?;

        if let Some(value) = parse_value(tokens, lc)? {
            values.push(value);
        } else {
            break;
        }
    }

    parse_newlines(tokens)?;

    if let Some((token, end_offset)) = tokens.next() {
        match token {
            Token::CloseParenthesis => Ok(Some(Ast::new(AstNode::List(values), start_offset, lc))),
            _ => Err(Error::new("Expected ')'", Some(end_offset), lc)),
        }
    } else {
        Err(Error::new("Expected ')'", None, lc))
    }
}

pub fn parse_brackets(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    match tokens.peek() {
        Some((Token::OpenBracket, offset)) => {
            Err(Error::new("Bracket syntax is reserved", Some(offset), lc))
        }
        _ => Ok(None),
    }
}

pub fn parse_name(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    parse_single_token!(Name, tokens, lc)
}

pub fn parse_text(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    parse_single_token!(Text, tokens, lc)
}

pub fn parse_number(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    parse_single_token!(Number, tokens, lc)
}

pub fn parse_literal(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    parse_prefix_token!(Quote => Literal, tokens, lc)
}

pub fn parse_escaped(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    parse_prefix_token!(Backslash => Escaped, tokens, lc)
}

fn parse_value(tokens: &mut Tokens, lc: &LineColLookup) -> Result<Option<Ast>> {
    let choices = &[
        parse_block,
        parse_list,
        parse_brackets,
        parse_name,
        parse_text,
        parse_number,
        parse_literal,
        parse_escaped,
    ];

    for choice in choices {
        if let Some(value) = choice(tokens, lc)? {
            return Ok(Some(value));
        }
    }

    Ok(None)
}

fn parse_newline(tokens: &mut Tokens) -> Result<Option<()>> {
    parse_single_token!(Newline, tokens)
}

fn parse_newlines(tokens: &mut Tokens) -> Result<()> {
    while parse_newline(tokens)?.is_some() {}

    Ok(())
}

impl SourceLocation {
    pub fn new(offset: &Range<usize>, lc: &LineColLookup) -> Self {
        let (line, column) = lc.get(offset.start);
        SourceLocation { line, column }
    }
}
