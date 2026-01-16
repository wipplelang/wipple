use crate::syntax::{ParseError, Parser, TokenKind};

pub fn parse_type_name(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    parser.token_with_name(TokenKind::CapitalName, "a type name")
}

pub fn parse_constructor_name(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    parser.token_with_name(TokenKind::CapitalName, "a constructor name")
}

pub fn parse_variable_name(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    parser.token_with_name(TokenKind::LowercaseName, "a variable name")
}

pub fn parse_type_parameter_name(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    parser.token_with_name(TokenKind::LowercaseName, "a type parameter name")
}

pub fn parse_attribute_name(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    if let Some(name) = parser.parse_optional(|parser| {
        parser.token_with_name(TokenKind::LowercaseName, "an attribute name")
    })? {
        return Ok(name);
    }

    if let Some(intrinsic) =
        parser.parse_optional(|parser| parser.token(TokenKind::IntrinsicKeyword))?
    {
        return Ok(intrinsic);
    }

    Err(parser.error("Expected an attribute name"))
}
