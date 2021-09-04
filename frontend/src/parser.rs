//! Parse Wipple source files.

use crate::diagnostics::Diagnostics;
use codemap_diagnostic::Level;
use logos::Logos;
use std::ops::Range;

parser_expression_kind! {
    /// The kind of a parsed expression.
    pub ExpressionKind of Self;
}

/// A parsed expression.
#[derive(Debug, Clone)]
pub struct Expression {
    /// The expression's location in the source code.
    pub location: Range<usize>,

    /// The kind of expression.
    pub kind: ExpressionKind,
}

#[derive(Logos)]
enum Token<'src> {
    #[token("'")]
    Literal,

    #[token(",")]
    Substitute,

    #[token("(")]
    LeftParenthesis,

    #[token(")")]
    RightParenthesis,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[regex(r#"\n+"#)]
    Newline,

    #[regex(r#"\t+"#)]
    Indent,

    #[regex(r#" +"#, logos::skip)]
    Space,

    #[regex(r#"[0-9]+(\.[0-9]+)?"#, |lex| lex.slice().parse())]
    Number(f64),

    #[regex(r#""[^"\\]*(?s:\\.[^"\\]*)*""#, |lex| &lex.slice()[1..lex.slice().len()])]
    Text(&'src str),

    #[error]
    Error,
}

fn unescape(literal: &str, diagnostics: &mut Diagnostics) -> Option<String> {
    let mut string = String::with_capacity(literal.len());
    let mut error = false;

    rustc_lexer::unescape::unescape_str(literal, &mut |range, result| match result {
        Ok(ch) => string.push(ch),
        Err(e) => {
            error = true;

            diagnostics.add(
                range,
                Level::Error,
                format!("Invalid character sequence in string literal ({:?})", e),
            )
        }
    });

    string.shrink_to_fit();

    error.then(|| string)
}
