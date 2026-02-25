use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Node, NodeRef},
    nodes::visit_type,
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct UnitTypeNode;

impl Node for UnitTypeNode {}

pub fn parse_unit_type(parser: &mut Parser<'_>) -> Result<UnitTypeNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;
    parser.token(TokenKind::RightParenthesis)?;
    Ok(UnitTypeNode)
}

impl Visit for UnitTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

        visitor.constraint(TypeConstraint::new(node.clone(), visitor.unit_type()));
    }
}

impl Codegen for UnitTypeNode {
    fn codegen(&self, _node: &NodeRef, _ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        None
    }
}
