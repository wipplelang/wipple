use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::visit_expression,
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct UnitExpressionNode;

impl Node for UnitExpressionNode {}

pub fn parse_unit_expression(parser: &mut Parser<'_>) -> Result<UnitExpressionNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(UnitExpressionNode)
}

impl Visit for UnitExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.constraint(TypeConstraint::new(node.clone(), visitor.unit_type()));
    }
}

impl Codegen for UnitExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Tuple(Vec::new()),
        });

        Ok(())
    }
}
