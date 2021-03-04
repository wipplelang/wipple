use crate::grammar::*;
use line_col::LineColLookup;

pub type Result = std::result::Result<AST, Error>;

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

pub fn parse(code: &str) -> Result {
    let code = String::from(code) + "\n";

    grammar::program(&code, &LineColLookup::new(&code)).map_err(|error| Error {
        message: error.to_string(),
        line: error.location.line,
        column: error.location.column,
    })
}
