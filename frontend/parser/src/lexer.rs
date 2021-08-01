use line_col::LineColLookup;
use logos::{Lexer, Logos};
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
    #[regex(r#"#!(.*)"#, shebang, priority = 3)]
    Shebang,

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| lex.slice().to_string(), priority = 2)]
    Number(String),

    #[regex(r#"[^ \t\n\(\)\[\]{}'\\"]+"#, |lex| lex.slice().to_string(), priority = 1)]
    Name(String),

    #[regex(r#""[^\n"]*""#, |lex| unescape(lex.slice()))]
    Text(String),

    #[token("'")]
    Quote,

    #[token("\\")]
    Backslash,

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

    #[regex("--.*", logos::skip)]
    Comment,

    #[error]
    Error,
}

fn shebang(lex: &mut Lexer<Token>) -> Result<(), String> {
    // Only match if shebang is at the start of the file
    if lex.span().start == 0 {
        Ok(())
    } else {
        Err("Shebang must be placed at the top of the file".to_string())
    }
}
