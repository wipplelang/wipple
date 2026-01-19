use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_atomic_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct IntrinsicExpressionNode {
    pub name: String,
    pub inputs: Vec<NodeRef>,
}

impl Node for IntrinsicExpressionNode {}

pub fn parse_intrinsic_expression(
    parser: &mut Parser<'_>,
) -> Result<IntrinsicExpressionNode, ParseError> {
    parser.token(TokenKind::IntrinsicKeyword)?;
    parser.commit("in this `intrinsic` expression");

    let name = parser.token(TokenKind::String)?;

    let inputs = if let Some(inputs) = parser.parse_optional(|parser| {
        parser.parse_many(0, parse_atomic_expression, |parser| parser.parse_nothing())
    })? {
        inputs.into_iter().map(|(node, _)| node).collect()
    } else {
        Vec::new()
    };

    Ok(IntrinsicExpressionNode { name, inputs })
}

impl Visit for IntrinsicExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        for input in &self.inputs {
            visitor.visit(input);
            visitor.edge(input, node, "input");
        }
    }
}

impl Codegen for IntrinsicExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let name = self.name.replace('-', "_");

        ctx.mark_reachable_intrinsic(&name);

        ctx.write_string(format!("await __wipple_runtime_{}(", name));

        for input in &self.inputs {
            ctx.write(input)?;
            ctx.write_string(", ");
        }

        ctx.write_string(")");

        Ok(())
    }
}
