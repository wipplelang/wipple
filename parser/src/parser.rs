use crate::grammar::*;
use line_col::LineColLookup;

pub type Result = std::result::Result<Ast, Error>;

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

macro_rules! parse_rule {
    ($code:expr, $rule:ident) => {{
        let code = String::from($code) + "\n";

        grammar::$rule(&code, &LineColLookup::new(&code)).map_err(|error| Error {
            message: error.to_string(),
            line: error.location.line,
            column: error.location.column,
        })
    }};
}

pub fn parse_file(code: &str) -> Result {
    parse_rule!(code, file)
}

pub fn parse_inline_program(code: &str) -> Result {
    parse_rule!(code, inline_program)
}
