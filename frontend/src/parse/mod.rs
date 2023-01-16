mod grammar;
mod lexer;
mod span;

pub use grammar::*;
pub use lexer::*;
use logos::Lexer;
pub use span::*;

use crate::{helpers::InternedString, Compiler, FilePath};

impl Compiler<'_> {
    pub fn parse(&self, file: FilePath, code: &str) -> File {
        let code = code.replace("\r\n", "\n");

        let (shebang, code) = match grammar::parse_shebang(&code) {
            Some((shebang, code)) => (Some(shebang), code),
            None => (None, code.as_str()),
        };

        let mut parser = Parser {
            compiler: self,
            lexer: Lexer::new(code).spanned().peekable(),
            len: code.len(),
            offset: shebang.map(|s| "#!".len() + s.len()).unwrap_or(0),
            file,
        };

        let (attributes, statements) = parser.parse_file();

        File {
            path: file,
            span: parser.file_span(),
            shebang: shebang.map(InternedString::new),
            attributes,
            statements,
        }
    }
}
