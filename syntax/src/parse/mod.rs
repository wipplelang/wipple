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

    let (comments, attributes, statements) = parser.parse_file();

    File {
        span: parser.file_span(),
        shebang: shebang.map(|s| parser.driver.intern(s)),
        comments,
        attributes,
        statements,
    }
}
