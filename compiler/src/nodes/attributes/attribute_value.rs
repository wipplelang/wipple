use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug)]
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
    (parser.try_node(parse_string_attribute_value)?)
        .ok_or_else(|| parser.error("Expected an attribute value"))
}

impl Visit for StringAttributeValueNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for StringAttributeValueNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
