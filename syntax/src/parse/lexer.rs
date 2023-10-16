use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'src> {
    #[token("[[")]
    LeftFileBracket,

    #[token("]]")]
    RightFileBracket,

    #[token("[")]
    LeftAttrBracket,

    #[token("]")]
    RightAttrBracket,

    #[token("(")]
    LeftParenthesis,

    #[token("'(")]
    QuoteLeftParenthesis,

    #[token("...(")]
    RepeatLeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("{")]
    LeftBrace,

    #[token("'{")]
    QuoteLeftBrace,

    #[token("...{")]
    RepeatLeftBrace,

    #[token("}")]
    RightBrace,

    #[token("_")]
    Underscore,

    #[regex(r#"\n"#)]
    LineBreak,

    #[regex(r#"\\\n"#)]
    LineContinue,

    #[regex(r#"[\t ]+"#, logos::skip)]
    Space,

    #[regex(r#"\{%[^\{\}%]*%\}"#, |lex| &lex.slice()[2..(lex.slice().len() - 2)])]
    Placeholder(&'src str),

    #[regex(r#"--.*"#, |lex| &lex.slice()[2..], priority = 2)]
    Comment(&'src str),

    #[regex(r#"'[A-Za-z0-9\-_]+[!?']?"#, |lex| &lex.slice()[1..])]
    QuoteName(&'src str),

    #[regex(r#"\.\.\.[A-Za-z0-9\-_]+[!?']?"#, |lex| &lex.slice()[3..])]
    RepeatName(&'src str),

    #[regex(r#"|[~`!@#$%^&*\-+=\\\|:;<,>.?/]+|[A-Za-z0-9\-_]+[!?']?"#, |lex| lex.slice())]
    Name(&'src str),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| &lex.slice()[1..(lex.slice().len() - 1)])]
    Text(&'src str),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| lex.slice(), priority = 2)]
    Number(&'src str),

    #[regex(r#"`[^`]*`"#, |lex| &lex.slice()[1..(lex.slice().len() - 1)])]
    Asset(&'src str),

    #[error]
    Error,
}

impl<'src> Token<'src> {
    pub fn as_bracket_str(&self) -> &'static str {
        match self {
            Token::LeftFileBracket => "[[",
            Token::RightFileBracket => "]]",
            Token::LeftAttrBracket => "[",
            Token::RightAttrBracket => "]",
            Token::LeftParenthesis => "(",
            Token::QuoteLeftParenthesis => "'(",
            Token::RepeatLeftParenthesis => "...(",
            Token::RightParenthesis => ")",
            Token::LeftBrace => "{",
            Token::QuoteLeftBrace => "'{",
            Token::RepeatLeftBrace => "...{",
            Token::RightBrace => "}",
            _ => panic!("not a bracket"),
        }
    }
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftFileBracket => write!(f, "`[[`"),
            Token::RightFileBracket => write!(f, "`]]`"),
            Token::LeftAttrBracket => write!(f, "`[`"),
            Token::RightAttrBracket => write!(f, "`]`"),
            Token::LeftParenthesis => write!(f, "`(`"),
            Token::QuoteLeftParenthesis => write!(f, "`'(`"),
            Token::RepeatLeftParenthesis => write!(f, "`...(`"),
            Token::RightParenthesis => write!(f, "`)`"),
            Token::LeftBrace => write!(f, "`{{`"),
            Token::QuoteLeftBrace => write!(f, "`'{{`"),
            Token::RepeatLeftBrace => write!(f, "`...{{`"),
            Token::RightBrace => write!(f, "`}}`"),
            Token::Underscore => write!(f, "`_`"),
            Token::LineBreak | Token::LineContinue => write!(f, "line break"),
            Token::Placeholder(placeholder) => write!(f, "{{%{placeholder}%}}"),
            Token::Space => unreachable!(),
            Token::Comment(_) => write!(f, "comment"),
            Token::QuoteName(name) => write!(f, "`'{name}`"),
            Token::RepeatName(name) => write!(f, "`...{name}`"),
            Token::Name(name) => write!(f, "`{name}`"),
            Token::Text(text) => write!(f, "`\"{text}\"`"),
            Token::Number(number) => write!(f, "`{number}`"),
            Token::Asset(_) => write!(f, "asset"),
            Token::Error => write!(f, "invalid token"),
        }
    }
}
