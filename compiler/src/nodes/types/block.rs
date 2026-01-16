use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_type_element, visit_type},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct BlockTypeNode {
    pub output: NodeRef,
}

impl Node for BlockTypeNode {}

pub fn parse_block(parser: &mut Parser<'_>) -> Result<BlockTypeNode, ParseError> {
    parser.token(TokenKind::LeftBrace)?;
    let output = parse_type_element(parser)?;
    parser.token(TokenKind::RightBrace)?;

    Ok(BlockTypeNode { output })
}

impl Visit for BlockTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

        visitor.visit(&self.output);
        visitor.edge(&self.output, node, "output");

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.block_type(self.output.clone()),
        ));
    }
}

impl Codegen for BlockTypeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
