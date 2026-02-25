use crate::{
    codegen::{Codegen, CodegenCtx, ir},
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

        for arm in &self.arms {
            visitor.matching(&input_temporary, true, false, |visitor| {
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

        visitor.insert(node, ResolvedWhen { input_temporary });
    }
}

impl Codegen for WhenExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let ResolvedWhen { input_temporary } = ctx.get::<ResolvedWhen>(node)?;

        let mut body = Vec::new();

        for temporary in self
            .arms
            .iter()
            .flat_map(|arm| ctx.db.temporaries(&arm.pattern))
            .collect::<BTreeSet<_>>()
        {
            if temporary == input_temporary {
                continue;
            }

            body.push(ir::Expression::Declare(temporary).at(node, ctx)?);
        }

        let mut arms = Vec::new();
        for arm in &self.arms {
            arms.push((
                ctx.codegen(&arm.pattern)?,
                Some(Box::new(ctx.codegen(&arm.value)?)),
            ));
        }

        body.push(ir::Expression::If(arms, None).at(node, ctx)?);

        ir::Expression::Call(
            Box::new(
                ir::Expression::Function(
                    vec![input_temporary],
                    Box::new(ir::Expression::Sequence(body).at(node, ctx)?),
                )
                .at(node, ctx)?,
            ),
            vec![ctx.codegen(&self.input)?],
        )
        .at(node, ctx)
    }
}
