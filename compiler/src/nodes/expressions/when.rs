use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{parse_atomic_expression, parse_atomic_pattern, parse_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};
use std::collections::BTreeSet;

#[derive(Debug, Clone)]
pub struct ResolvedWhen {
    pub input_temporary: NodeRef,
}

impl Fact for ResolvedWhen {}

impl Render for ResolvedWhen {}

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

        let span = visitor.span(&self.input);
        let input_temporary = visitor.node(span, HiddenNode(None));

        visitor.constraint(GroupConstraint::new(
            input_temporary.clone(),
            self.input.clone(),
        ));

        visitor.matching(input_temporary.clone(), false, |visitor| {
            for arm in &self.arms {
                visitor.push_scope();
                visitor.visit(&arm.pattern);
                visitor.edge(&arm.pattern, node, "pattern");
                visitor.visit(&arm.value);
                visitor.edge(&arm.value, node, "value");
                visitor.pop_scope();

                visitor.constraint(GroupConstraint::new(arm.value.clone(), node.clone()));
            }
        });

        visitor.insert(node, ResolvedWhen { input_temporary });
    }
}

impl Codegen for WhenExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let ResolvedWhen { input_temporary } = ctx
            .db
            .get::<ResolvedWhen>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        ctx.write_string("await (async (");
        ctx.write_node(&input_temporary);
        ctx.write_string(") => {");
        ctx.write_line();

        for temporary in self
            .arms
            .iter()
            .flat_map(|arm| ctx.db.temporaries(&arm.pattern))
            .collect::<BTreeSet<_>>()
        {
            if temporary == input_temporary {
                continue;
            }

            ctx.write_string("var ");
            ctx.write_node(&temporary);
            ctx.write_string(";");
            ctx.write_line();
        }

        for arm in &self.arms {
            ctx.write_string("if (true");
            ctx.write(&arm.pattern)?;
            ctx.write_string(") {");
            ctx.write_line();
            ctx.write_string("return ");
            ctx.write(&arm.value)?;
            ctx.write_string(";");
            ctx.write_line();
            ctx.write_string("}");
            ctx.write_line();
        }

        ctx.write_string("throw new Error(\"unreachable\");");
        ctx.write_line();

        ctx.write_string("})(");
        ctx.write(&self.input)?;
        ctx.write_string(")");

        Ok(())
    }
}
