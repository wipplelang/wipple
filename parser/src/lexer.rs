use line_col::LineColLookup;
use logos::Logos;
use snailquote::unescape;
use std::{iter::Peekable, ops::Range, slice::Iter};

pub type Tokens<'a> = Peekable<Iter<'a, (Token, Range<usize>)>>;

pub fn lex(code: &str) -> (Vec<(Token, Range<usize>)>, LineColLookup) {
    (
        Token::lexer(code).spanned().collect(),
        LineColLookup::new(code),
    )
}

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex(r#"[^ \t\n\(\)\[\]{}'"]+"#, |lex| lex.slice().to_string(), priority = 1)]
    Name(String),

    #[regex(r"-?[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse(), priority = 2)]
    Number(f64),

    #[regex(r#""[^\n"]*""#, |lex| unescape(lex.slice()))]
    Text(String),

    #[token("'")]
    Quote,

    #[token("(")]
    OpenParenthesis,

    #[token(")")]
    CloseParenthesis,

    #[token("[")]
    OpenBracket,

    #[token("]")]
    CloseBracket,

    #[token("{")]
    OpenBrace,

    #[token("}")]
    CloseBrace,

    #[regex(r"[ \t]+", logos::skip)]
    Whitespace,

    #[regex("\n")]
    Newline,

    #[regex("--.*")]
    Comment,

    #[error]
    Error,
}
