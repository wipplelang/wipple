use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_atomic_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct DoExpressionNode {
    pub input: NodeRef,
}

impl Node for DoExpressionNode {}

pub fn parse_do_expression(parser: &mut Parser<'_>) -> Result<DoExpressionNode, ParseError> {
    parser.token(TokenKind::DoKeyword)?;
    parser.commit("in this `do` expression");

    let input = parse_atomic_expression(parser)?;

    Ok(DoExpressionNode { input })
}

impl Visit for DoExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.visit(&self.input);
        visitor.edge(&self.input, node, "input");

        visitor.constraint(TypeConstraint::new(
            self.input.clone(),
            visitor.block_type(node.clone()),
        ));
    }
}

impl Codegen for DoExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string("await (");
        ctx.write(&self.input)?;
        ctx.write_string(")()");

        Ok(())
    }
}
