use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{HiddenNode, Node, NodeRef},
    nodes::{Matching, NamedTypeNode, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{MatchPathSegment, Visit, Visitor},
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
        visit_pattern(node, visitor, Some(MatchPathSegment::NoMatch));

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
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.condition(ir::Condition::EqualToString {
            input: matching,
            value: self.value.clone(),
        });

        Ok(())
    }
}
