use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::visit_type,
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct PlaceholderTypeNode;

impl Node for PlaceholderTypeNode {}

pub fn parse_placeholder_type(parser: &mut Parser<'_>) -> Result<PlaceholderTypeNode, ParseError> {
    parser.token(TokenKind::UnderscoreKeyword)?;
    Ok(PlaceholderTypeNode)
}

impl Visit for PlaceholderTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);
    }
}

impl Codegen for PlaceholderTypeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
