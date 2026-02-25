use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Fact, Node, NodeRef, Render},
    nodes::visit_expression,
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct IsPlaceholder;

impl Fact for IsPlaceholder {}

impl Render for IsPlaceholder {}

#[derive(Debug)]
pub struct PlaceholderExpressionNode;

impl Node for PlaceholderExpressionNode {}

pub fn parse_placeholder_expression(
    parser: &mut Parser<'_>,
) -> Result<PlaceholderExpressionNode, ParseError> {
    parser.token(TokenKind::UnderscoreKeyword)?;
    parser.commit("in this placeholder expression");

    Ok(PlaceholderExpressionNode)
}

impl Visit for PlaceholderExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);
        visitor.insert(node, IsPlaceholder);
    }
}

impl Codegen for PlaceholderExpressionNode {
    fn codegen(&self, _node: &NodeRef, _ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        None
    }
}
