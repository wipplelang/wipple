use crate::attributes::attribute_value::parse_attribute_value;

use wipple_core::{ast::AstKey, visit::attributes::Attribute};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_attribute_name,
    parser::{ParseError, Parser},
};

pub fn parse_attributes(parser: &mut Parser<'_>) -> Result<Vec<AstKey>, ParseError> {
    parser.parse_lines(0, false, parse_attribute)
}

pub fn parse_attribute(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LeftBracket)?;

    let name = parse_attribute_name(parser)?;

    let value = parser.parse_optional(|parser| {
        parser.token(TokenKind::AssignOperator)?;
        parser.consume_line_breaks();
        parse_attribute_value(parser)
    })?;

    parser.token(TokenKind::RightBracket)?;

    Ok(parser.in_ast(Attribute {
        span: span(parser),
        name,
        value,
    }))
}
