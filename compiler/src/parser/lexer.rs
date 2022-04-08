use crate::helpers::InternedString;
use logos::Logos;
use rust_decimal::Decimal;
use std::fmt;

#[derive(Logos, Debug, Clone, Copy)]
pub enum Token {
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("[:")]
    LeftColonBracket,

    #[token(":]")]
    RightColonBracket,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token("->")]
    Arrow,

    #[token("=>")]
    DoubleArrow,

    #[token("_")]
    Underscore,

    #[token("'")]
    Quote,

    #[token("use")]
    Use,

    #[token("when")]
    When,

    #[token("type")]
    Type,

    #[token("trait")]
    Trait,

    #[token("instance")]
    Instance,

    #[token("where")]
    Where,

    #[token("external")]
    External,

    #[regex(r#"[^\r\n\t \(\)\[\]\{\}'"/]+"#, |lex| InternedString::new(lex.slice()))]
    Name(InternedString),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| InternedString::new(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Text(InternedString),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| lex.slice().parse(), priority = 2)]
    Number(Decimal),

    #[regex(r#"[\r\n]+"#)]
    LineBreak,

    #[regex(r#"[ \t]+"#, logos::skip)]
    Space,

    #[regex(r#"--.*"#, logos::skip)]
    Comment,

    #[error]
    Error,
}

// TODO: Return iterator instead
pub fn lex(code: &str) -> impl Iterator<Item = (Token, logos::Span)> + '_ {
    logos::Lexer::new(code).spanned()
}

// TODO: LINE BREAK TOKEN

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "`(`"),
            Token::RightParen => write!(f, "`)`"),
            Token::LeftBracket => write!(f, "`[`"),
            Token::RightBracket => write!(f, "`]`"),
            Token::LeftBrace => write!(f, "`{{`"),
            Token::RightBrace => write!(f, "`}}`"),
            Token::LeftColonBracket => write!(f, "`[:`"),
            Token::RightColonBracket => write!(f, "`:]`"),
            Token::Colon => write!(f, "`:`"),
            Token::DoubleColon => write!(f, "`::`"),
            Token::Arrow => write!(f, "`->`"),
            Token::DoubleArrow => write!(f, "`=>`"),
            Token::Underscore => write!(f, "`_`"),
            Token::Quote => write!(f, "`'`"),
            Token::Use => write!(f, "`use`"),
            Token::When => write!(f, "`when`"),
            Token::Type => write!(f, "`type`"),
            Token::Trait => write!(f, "`trait`"),
            Token::Instance => write!(f, "`instance`"),
            Token::Where => write!(f, "`where`"),
            Token::External => write!(f, "`external`"),
            Token::Name(_) => write!(f, "name"),
            Token::Text(_) => write!(f, "text"),
            Token::Number(_) => write!(f, "number"),
            Token::LineBreak => write!(f, "line break"),
            Token::Space | Token::Comment => unreachable!(),
            Token::Error => write!(f, "invalid token"),
        }
    }
}
