use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::{parse_atomic_expression, parse_atomic_pattern, parse_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct WhenExpressionNode {
    pub input: NodeRef,
    pub arms: Vec<WhenArm>,
}

#[derive(Debug)]
pub struct WhenArm {
    pub pattern: NodeRef,
    pub value: NodeRef,
}

impl Node for WhenExpressionNode {}

pub fn parse_when_expression(parser: &mut Parser<'_>) -> Result<WhenExpressionNode, ParseError> {
    parser.token(TokenKind::WhenKeyword)?;
    parser.commit("in this `when` expression");

    let input = parse_atomic_expression(parser)?;

    parser.token(TokenKind::LeftBrace)?;
    let arms = parse_arms(parser)?;
    parser.token(TokenKind::RightBrace)?;

    Ok(WhenExpressionNode { input, arms })
}

pub fn parse_arm(parser: &mut Parser<'_>) -> Result<WhenArm, ParseError> {
    let pattern = parse_atomic_pattern(parser)?;

    parser.commit("in this `when` arm");
    parser.token(TokenKind::FunctionOperator)?;
    parser.consume_line_breaks();

    let value = parse_expression(parser)?;

    Ok(WhenArm { pattern, value })
}

pub fn parse_arms(parser: &mut Parser<'_>) -> Result<Vec<WhenArm>, ParseError> {
    parser.parse_lines(0, true, parse_arm)
}

impl Visit for WhenExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.visit(&self.input);
        visitor.edge(&self.input, node, "input");

        for arm in &self.arms {
            visitor.matching(&self.input, true, false, |visitor| {
                visitor.current_match().root = Some(self.input.clone());
                visitor.current_match().arm = Some(arm.pattern.clone());

                visitor.push_scope();
                visitor.visit(&arm.pattern);
                visitor.edge(&arm.pattern, node, "pattern");
                visitor.visit(&arm.value);
                visitor.edge(&arm.value, node, "value");
                visitor.pop_scope();

                visitor.constraint(GroupConstraint::new(arm.value.clone(), node.clone()));
            });
        }
    }
}

impl Codegen for WhenExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        ctx.codegen(&self.input)?;

        let branches = self
            .arms
            .iter()
            .map(|arm| {
                ctx.push_conditions();
                ctx.codegen(&arm.pattern)?;
                let conditions = ctx.pop_conditions();

                ctx.push_instructions();
                ctx.codegen(&arm.value)?;
                let instructions = ctx.pop_instructions();

                Ok((conditions, instructions, Some(arm.value.clone())))
            })
            .collect::<CodegenResult<Vec<_>>>()?;

        ctx.instruction(ir::Instruction::If {
            node: Some(node.clone()),
            branches,
            else_branch: None,
        });

        Ok(())
    }
}
