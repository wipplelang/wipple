mod format;
mod grammar;
mod lexer;

pub use format::*;
pub use grammar::*;
pub use lexer::*;

use crate::Driver;
use logos::Lexer;

pub fn parse<D: Driver>(driver: &D, path: D::Path, code: &str) -> File<D> {
    let code = code.replace("\r\n", "\n");

    let (shebang, code) = match grammar::parse_shebang(&code) {
        Some((shebang, code)) => (Some(shebang), code),
        None => (None, code.as_str()),
    };

    let mut parser = Parser {
        driver,
        path,
        lexer: Lexer::new(code).spanned().peekable(),
        len: code.len(),
        offset: shebang.map(|s| "#!".len() + s.len()).unwrap_or(0),
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
