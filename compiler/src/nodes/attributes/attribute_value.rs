use crate::{
    codegen::Codegen,
    database::{Node, NodeRef},
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct StringAttributeValueNode {
    pub value: String,
}

impl Node for StringAttributeValueNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_string_attribute_value(
    parser: &mut Parser<'_>,
) -> Result<StringAttributeValueNode, ParseError> {
    let value = parser.token(TokenKind::String)?;

    Ok(StringAttributeValueNode { value })
}

pub fn parse_attribute_value(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_string_attribute_value)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_connection_attribute_value)? {
            return Ok(node);
        }

        Err(parser.error("Expected an attribute value"))
    })
}

impl Visit for StringAttributeValueNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for StringAttributeValueNode {}

#[derive(Debug, Clone)]
pub struct ConnectionAttributeValueNode {
    pub left: Option<String>,
    pub right: Option<String>,
    pub label: String,
}

impl Node for ConnectionAttributeValueNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_connection_attribute_value(
    parser: &mut Parser<'_>,
) -> Result<ConnectionAttributeValueNode, ParseError> {
    let left = parse_connection_name(parser)?;
    parser.token(TokenKind::FunctionOperator)?;
    let right = parse_connection_name(parser)?;
    parser.token(TokenKind::LeftParenthesis)?;
    let label = parser.token(TokenKind::String)?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(ConnectionAttributeValueNode { left, right, label })
}

fn parse_connection_name(parser: &mut Parser<'_>) -> Result<Option<String>, ParseError> {
    if let Some(name) = parser.parse_optional(|parser| {
        parser.token_with_name(TokenKind::LowercaseName, "a connection name")
    })? {
        return Ok(Some(name));
    }

    if parser
        .parse_optional(|parser| parser.token(TokenKind::UnderscoreKeyword))?
        .is_some()
    {
        return Ok(None);
    }

    Err(parser.error("Expected a connection name"))
}

impl Visit for ConnectionAttributeValueNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for ConnectionAttributeValueNode {}
