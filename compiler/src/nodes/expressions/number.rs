use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{HiddenNode, Node, NodeRef},
    nodes::{NamedTypeNode, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct NumberExpressionNode {
    pub value: String,
}

impl Node for NumberExpressionNode {}

pub fn parse_number_expression(
    parser: &mut Parser<'_>,
) -> Result<NumberExpressionNode, ParseError> {
    let value = parser.token(TokenKind::Number)?;

    Ok(NumberExpressionNode { value })
}

impl Visit for NumberExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let span = visitor.span(node);
        let number_type = visitor.node(
            span,
            HiddenNode::new(NamedTypeNode {
                name: String::from("Number"),
                parameters: Vec::new(),
            }),
        );

        visitor.visit(&number_type);
        visitor.constraint(GroupConstraint::new(node.clone(), number_type));
    }
}

impl Codegen for NumberExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string(&self.value);
        Ok(())
    }
}
