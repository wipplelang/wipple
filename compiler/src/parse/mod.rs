mod grammar;
mod lexer;
mod span;

pub use grammar::*;
pub use lexer::*;
use logos::Lexer;
pub use span::*;

use crate::{helpers::InternedString, Compiler, FilePath, Loader};

impl<L: Loader> Compiler<L> {
    pub fn parse_v2(&mut self, file: FilePath, code: &str) -> Option<File> {
        let (shebang, code) = match grammar::parse_shebang(code) {
            Some((shebang, code)) => (Some(shebang), code),
            None => (None, code),
        };

        let mut parser = Parser {
            lexer: Lexer::new(code).spanned().peekable(),
            len: code.len(),
            offset: shebang.map(|s| "#!".len() + s.len()).unwrap_or(0),
            file,
            diagnostics: &mut self.diagnostics,
        };

        parser.parse_file().map(|statements| File {
            path: file,
            span: parser.file_span(),
            shebang: shebang.map(InternedString::new),
            statements,
        })
    }
}