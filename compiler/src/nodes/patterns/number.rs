use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{HiddenNode, Node, NodeRef},
    nodes::{Matching, NamedTypeNode, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{MatchPathSegment, Visit, Visitor},
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
        visit_pattern(node, visitor, Some(MatchPathSegment::NoMatch));

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
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.condition(ir::Condition::EqualToNumber {
            input: matching,
            value: self.value.clone(),
        });

        Ok(())
    }
}
