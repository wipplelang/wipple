use wipple_core::{
    ast::AstKey,
    visit::attributes::{ConnectionAttributeValue, StringAttributeValue},
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_type_parameter_name,
    parse_alt,
    parser::{ParseError, Parser},
};

pub fn parse_attribute_value(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_string_attribute_value as value => parser.in_ast(value),
        parse_connection_attribute_value as value => parser.in_ast(value),
        _ => "Expected an attribute value",
    })
}

pub fn parse_string_attribute_value(
    parser: &mut Parser<'_>,
) -> Result<StringAttributeValue, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::String)?;
    Ok(StringAttributeValue {
        span: span(parser),
        value,
    })
}

pub fn parse_connection_attribute_value(
    parser: &mut Parser<'_>,
) -> Result<ConnectionAttributeValue, ParseError> {
    let span = parser.spanned();
    let left = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::FunctionOperator)?;
    let right = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::LeftParenthesis)?;
    let label = parser.token(TokenKind::String)?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(ConnectionAttributeValue {
        span: span(parser),
        left,
        right,
        label,
    })
}
