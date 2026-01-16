use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::visit_pattern,
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct UnitPatternNode;

impl Node for UnitPatternNode {}

pub fn parse_unit_pattern(parser: &mut Parser<'_>) -> Result<UnitPatternNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(UnitPatternNode)
}

impl Visit for UnitPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

        visitor.constraint(TypeConstraint::new(node.clone(), visitor.unit_type()));
    }
}

impl Codegen for UnitPatternNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        // No code needed
        Ok(())
    }
}
