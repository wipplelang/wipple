use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Node, NodeRef},
    nodes::{HasTemporaries, Matching, visit_pattern},
    syntax::{ParseError, Parser, parse_variable_name},
    visit::{MatchPathSegment, VariableDefinition, Visit, Visitor},
};

#[derive(Debug)]
pub struct VariablePatternNode {
    pub variable: String,
}

impl Node for VariablePatternNode {}

pub fn parse_variable_pattern(parser: &mut Parser<'_>) -> Result<VariablePatternNode, ParseError> {
    let variable = parse_variable_name(parser)?;

    Ok(VariablePatternNode { variable })
}

impl Visit for VariablePatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor, Some(MatchPathSegment::Match));

        let value = visitor.current_match().value.clone();

        visitor.define(
            &self.variable,
            VariableDefinition {
                name: self.variable.clone(),
                node: node.clone(),
                value,
            },
        );

        visitor.insert(node, HasTemporaries(vec![node.clone()]));
    }
}

impl Codegen for VariablePatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let Matching(matching) = ctx.get(node)?;

        ir::Expression::AssignTo(
            Box::new(ir::Expression::Variable(matching).at(node, ctx)?),
            node.clone(),
        )
        .at(node, ctx)
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}
