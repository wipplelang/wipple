use crate::{
    lexer::TokenKind,
    parser::{ParseError, ParseToken, Parser},
};
use wipple_core::span::Str;

pub fn parse_type_name(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    parser.token(ParseToken::from(TokenKind::CapitalName).name("a type name"))
}

pub fn parse_constructor_name(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    parser.token(ParseToken::from(TokenKind::CapitalName).name("a constructor name"))
}

pub fn parse_variable_name(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    parser.token(ParseToken::from(TokenKind::LowercaseName).name("a variable name"))
}

pub fn parse_type_parameter_name(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    parser.token(ParseToken::from(TokenKind::LowercaseName).name("a type parameter name"))
}

pub fn parse_attribute_name(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    if let Some(name) = parser.parse_optional(|parser| {
        parser.token(ParseToken::from(TokenKind::LowercaseName).name("an attribute name"))
    })? {
        return Ok(name);
    }

    if let Some(intrinsic) =
        parser.parse_optional(|parser| parser.token(TokenKind::IntrinsicKeyword))?
    {
        return Ok(intrinsic);
    }

    Err(parser.error("Expected an attribute name", None))
}
