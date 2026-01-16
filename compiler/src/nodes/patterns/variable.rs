use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{HasTemporaries, Matching, visit_pattern},
    syntax::{ParseError, Parser, parse_variable_name},
    visit::{VariableDefinition, Visit, Visitor},
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
        visit_pattern(node, visitor);

        visitor.define(
            &self.variable,
            VariableDefinition {
                name: self.variable.clone(),
                node: node.clone(),
                value: visitor.current_match().node.clone(),
            },
        );

        visitor.insert(node, HasTemporaries(vec![node.clone()]));
    }
}

impl Codegen for VariablePatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let Matching(matching) = ctx
            .db
            .get::<Matching>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string(" && ((");
        ctx.write_node(&ctx.current_node().clone());
        ctx.write_string(" = ");
        ctx.write_node(&matching);
        ctx.write_string(") || true)");

        Ok(())
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}
