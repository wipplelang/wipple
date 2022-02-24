use crate::helpers::InternedString;
use logos::Logos;
use std::{cmp::Ordering, fmt};

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

    #[token("/")]
    Slash,

    #[token("when")]
    When,

    #[token("type")]
    Type,

    #[token("trait")]
    Trait,

    #[regex(r#"[^\r\n\t \(\)\[\]\{\}'"/]+"#, |lex| InternedString::new(lex.slice()))]
    Name(InternedString),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| InternedString::new(&lex.slice()[1..(lex.slice().len() - 1)]))]
    Text(InternedString),

    #[regex(r#"-?[0-9]+(\.[0-9]+)?"#, |lex| lex.slice().parse(), priority = 2)]
    Number(f64),

    #[regex(r#"[\r\n]+\t*"#, |lex| lex.slice().chars().filter(|&c| c == '\t').count())]
    _IndentedLineBreak(usize),

    #[regex(r#" +"#, logos::skip)]
    Space,

    #[regex(r#"--.*"#, logos::skip)]
    Comment,

    LineBreak,
    Indent,
    Dedent,

    #[error]
    Error,
}

// TODO: Return iterator instead
pub fn lex(code: &str) -> Vec<(Token, logos::Span)> {
    process_indentation(code, logos::Lexer::new(code).spanned())
}

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
            Token::Slash => write!(f, "`/`"),
            Token::When => write!(f, "`when`"),
            Token::Type => write!(f, "`type`"),
            Token::Trait => write!(f, "`trait`"),
            Token::Name(_) => write!(f, "name"),
            Token::Text(_) => write!(f, "text"),
            Token::Number(_) => write!(f, "number"),
            Token::LineBreak => write!(f, "line break"),
            Token::Indent => write!(f, "indent"),
            Token::Dedent => write!(f, "dedent"),
            Token::Space | Token::Comment | Token::_IndentedLineBreak(_) => unreachable!(),
            Token::Error => write!(f, "invalid token"),
        }
    }
}

fn process_indentation(
    code: &str,
    tokens: impl Iterator<Item = (Token, logos::Span)>,
) -> Vec<(Token, logos::Span)> {
    let mut current_indent_level = 0usize;
    let mut current_paren_level = 0usize;
    let eof_span = code.len()..code.len();

    let mut result = Vec::new();
    for (token, span) in tokens {
        match token {
            Token::_IndentedLineBreak(indent_level) => {
                if current_paren_level == 0 {
                    match indent_level.cmp(&current_indent_level) {
                        Ordering::Greater => {
                            for _ in 0..(indent_level - current_indent_level) {
                                result.push((Token::Indent, span.clone()));
                            }
                        }
                        Ordering::Less => {
                            for _ in 0..(current_indent_level - indent_level) {
                                result.push((Token::Dedent, span.clone()));
                            }
                        }
                        Ordering::Equal => {}
                    }

                    current_indent_level = indent_level;
                }

                result.push((Token::LineBreak, span));
            }
            _ => {
                match token {
                    Token::LeftParen => {
                        current_paren_level += 1;
                    }
                    Token::RightParen => {
                        current_paren_level = current_paren_level.saturating_sub(1);
                    }
                    _ => {}
                }

                result.push((token, span));
            }
        }
    }

    for _ in 0..current_indent_level {
        result.push((Token::Dedent, eof_span.clone()));
    }

    result
}
