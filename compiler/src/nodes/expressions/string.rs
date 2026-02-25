use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{HiddenNode, Node, NodeRef},
    nodes::{NamedTypeNode, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct StringExpressionNode {
    pub value: String,
}

impl Node for StringExpressionNode {}

pub fn parse_string_expression(
    parser: &mut Parser<'_>,
) -> Result<StringExpressionNode, ParseError> {
    let value = parser.token(TokenKind::String)?;

    Ok(StringExpressionNode { value })
}

impl Visit for StringExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let span = visitor.span(node);
        let string_type = visitor.node(
            span,
            HiddenNode::new(NamedTypeNode {
                name: String::from("String"),
                parameters: Vec::new(),
            }),
        );

        visitor.visit(&string_type);
        visitor.constraint(GroupConstraint::new(node.clone(), string_type));
    }
}

impl Codegen for StringExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        ir::Expression::String(self.value.clone()).at(node, ctx)
    }
}
