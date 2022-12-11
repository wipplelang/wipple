use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'src> {
    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[[")]
    LeftFileBracket,

    #[token("]]")]
    RightFileBracket,

    #[token("[")]
    LeftAttrBracket,

    #[token("]")]
    RightAttrBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("_")]
    Underscore,

    #[regex(r#"\n+"#)]
    LineBreak,

    #[regex(r#"\t+"#, |lex| lex.slice().len() as u32)]
    Indent(u32),

    #[regex(r#" +"#, logos::skip)]
    Space,

    #[regex(r#"--.*"#, |lex| &lex.slice()[2..], priority = 2)]
    Comment(&'src str),

    #[regex(r#"[^\n\t \(\)\[\]\{\}"]+"#, |lex| lex.slice())]
    Name(&'src str),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| &lex.slice()[1..(lex.slice().len() - 1)])]
    Text(&'src str),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| lex.slice(), priority = 2)]
    Number(&'src str),

    #[error]
    Error,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftParenthesis => write!(f, "`(`"),
            Token::RightParenthesis => write!(f, "`)`"),
            Token::LeftFileBracket => write!(f, "`[[`"),
            Token::RightFileBracket => write!(f, "`]]`"),
            Token::LeftAttrBracket => write!(f, "`[`"),
            Token::RightAttrBracket => write!(f, "`]`"),
            Token::LeftBrace => write!(f, "`{{`"),
            Token::RightBrace => write!(f, "`}}`"),
            Token::Underscore => write!(f, "`_`"),
            Token::LineBreak => write!(f, "line break"),
            Token::Indent(_) => write!(f, "indent"),
            Token::Space => unreachable!(),
            Token::Comment(_) => write!(f, "comment"),
            Token::Text(text) => write!(f, "`\"{}\"`", text),
            Token::Number(number) => write!(f, "`{}`", number),
            Token::Name(name) => write!(f, "`{}`", name),
            Token::Error => write!(f, "invalid token"),
        }
    }
}
