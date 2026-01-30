use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_operator_expression, parse_type_element, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct AnnotateExpressionNode {
    pub expression: NodeRef,
    pub ty: NodeRef,
}

impl Node for AnnotateExpressionNode {}

pub fn parse_annotate_expression(
    parser: &mut Parser<'_>,
) -> Result<AnnotateExpressionNode, ParseError> {
    let expression = parse_operator_expression(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let ty = parse_type_element(parser)?;

    Ok(AnnotateExpressionNode { expression, ty })
}

impl Visit for AnnotateExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.visit(&self.expression);
        visitor.visit(&self.ty);

        visitor.graph.edge(&self.expression, node, "annotated");
        visitor.graph.edge(&self.ty, node, "type");

        visitor.constraint(GroupConstraint::new(
            self.expression.clone(),
            self.ty.clone(),
        ));
        visitor.constraint(GroupConstraint::new(node.clone(), self.expression.clone()));
    }
}

impl Codegen for AnnotateExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write(&self.expression)
    }
}
