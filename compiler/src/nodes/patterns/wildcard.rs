use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::visit_pattern,
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct WildcardPatternNode;

impl Node for WildcardPatternNode {}

pub fn parse_wildcard_pattern(parser: &mut Parser<'_>) -> Result<WildcardPatternNode, ParseError> {
    parser.token(TokenKind::UnderscoreKeyword)?;
    parser.commit("in this wildcard pattern");

    Ok(WildcardPatternNode)
}

impl Visit for WildcardPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);
    }
}

impl Codegen for WildcardPatternNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        // No code needed
        Ok(())
    }
}
