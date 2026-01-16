use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{HiddenNode, Node, NodeRef},
    nodes::{Matching, NamedTypeNode, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct NumberPatternNode {
    pub value: String,
}

impl Node for NumberPatternNode {}

pub fn parse_number_pattern(parser: &mut Parser<'_>) -> Result<NumberPatternNode, ParseError> {
    let value = parser.token(TokenKind::Number)?;

    Ok(NumberPatternNode { value })
}

impl Visit for NumberPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

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

impl Codegen for NumberPatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let Matching(matching) = ctx
            .db
            .get::<Matching>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string(" && (");
        ctx.write_node(&matching);
        ctx.write_string(" === ");
        ctx.write_string(&self.value);
        ctx.write_string(")");

        Ok(())
    }
}
