use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::{IsMutated, Matching, Temporaries, visit_pattern},
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
        let mutable = visitor.current_match().mutable;

        visitor.define(
            &self.variable,
            VariableDefinition {
                name: self.variable.clone(),
                node: node.clone(),
                value,
                mutable,
            },
        );

        visitor.insert(
            node,
            Temporaries {
                has: vec![node.clone()],
                inherit: Vec::new(),
            },
        )
    }
}

impl Codegen for VariablePatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        let mutable = ctx.get::<IsMutated>(node).is_some();

        ctx.condition(ir::Condition::Initialize {
            variable: node.clone(),
            node: Some(matching.clone()),
            value: ir::Value::Variable(matching),
            mutable,
        });

        Ok(())
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}
