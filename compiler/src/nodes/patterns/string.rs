use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{HiddenNode, Node, NodeRef},
    nodes::{Matching, NamedTypeNode, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct StringPatternNode {
    pub value: String,
}

impl Node for StringPatternNode {}

pub fn parse_string_pattern(parser: &mut Parser<'_>) -> Result<StringPatternNode, ParseError> {
    let value = parser.token(TokenKind::String)?;

    Ok(StringPatternNode { value })
}

impl Visit for StringPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

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

impl Codegen for StringPatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let Matching(matching) = ctx
            .db
            .get::<Matching>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string(" && (");
        ctx.write_node(&matching);
        ctx.write_string(" === ");
        ctx.write_string(serde_json::to_string(&self.value).unwrap());
        ctx.write_string(")");

        Ok(())
    }
}
