mod format;
mod grammar;
mod lexer;

pub use format::*;
pub use grammar::*;
pub use lexer::*;

use crate::{CharIndex, Driver};
use logos::Lexer;

#[derive(Debug, Clone, Copy, Default)]
#[non_exhaustive]
pub struct Options {}

pub fn parse<D: Driver>(
    driver: &D,
    path: D::InternedString,
    code: &str,
    Options {}: Options,
) -> File<D> {
    let code = code.replace("\r\n", "\n");

    let (shebang, code) = match grammar::parse_shebang(&code) {
        Some((shebang, code)) => (Some(shebang), code),
        None => (None, code.as_str()),
    };

    let mut parser = Parser {
        driver,
        path,
        lexer: LexerIter::new(Lexer::new(code).spanned()).peekable(),
        len: CharIndex {
            utf8: code.len(),
            utf16: code.encode_utf16().count(),
        },
        offset: shebang
            .map(|s| CharIndex {
                utf8: "#!".len() + s.len(),
                utf16: "#!".encode_utf16().count() + s.encode_utf16().count(),
            })
            .unwrap_or(CharIndex::ZERO),
    };

    let FileContents {
        comments,
        attributes,
        statements,
    } = parser.parse_file();

    File {
        span: parser.file_span(),
        shebang: shebang.map(|s| parser.driver.intern(s)),
        comments,
        attributes,
        statements,
    }
}

pub fn substitute<D: Driver>(
    expr: &mut Expr<D>,
    replacement_name: D::InternedString,
    replacement: Expr<D>,
) {
    match &mut expr.kind {
        ExprKind::QuoteName(name) => {
            if *name == replacement_name {
                *expr = replacement.clone();
            }
        }
        ExprKind::List(lines) | ExprKind::RepeatList(lines) | ExprKind::RepeatBlock(lines) => {
            for line in lines {
                for expr in &mut line.exprs {
                    substitute(expr, replacement_name.clone(), replacement.clone());
                }
            }
        }
        ExprKind::Block(statements) => {
            for statement in statements {
                for line in &mut statement.lines {
                    for expr in &mut line.exprs {
                        substitute(expr, replacement_name.clone(), replacement.clone());
                    }
                }
            }
        }
        ExprKind::Placeholder(_)
        | ExprKind::Underscore
        | ExprKind::Name(_, _)
        | ExprKind::RepeatName(_)
        | ExprKind::Text(_)
        | ExprKind::Number(_)
        | ExprKind::Asset(_)
        | ExprKind::SourceCode(_) => {}
    }
}
