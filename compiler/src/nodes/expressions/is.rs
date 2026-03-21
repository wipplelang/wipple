use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{
        ConstructorExpressionNode, parse_expression_element, parse_pattern_element,
        visit_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedIs {
    pub true_variant: NodeRef,
    pub false_variant: NodeRef,
}

impl Fact for ResolvedIs {}

impl Render for ResolvedIs {}

#[derive(Debug)]
pub struct IsExpressionNode {
    pub left: NodeRef,
    pub right: NodeRef,
}

impl Node for IsExpressionNode {}

pub fn parse_is_expression(parser: &mut Parser<'_>) -> Result<IsExpressionNode, ParseError> {
    let left = parse_expression_element(parser)?;
    parser.token(TokenKind::IsOperator)?;
    parser.consume_line_breaks();
    let right = parse_pattern_element(parser)?;

    Ok(IsExpressionNode { left, right })
}

impl Visit for IsExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.visit(&self.left);
        visitor.edge(&self.left, node, "left");

        let span = visitor.span(node);

        visitor.matching(&self.left, true, false, |visitor| {
            visitor.current_match().root = Some(self.left.clone());
            visitor.visit(&self.right);
            visitor.edge(&self.right, node, "right");
        });

        let true_node = visitor.node(
            span.clone(),
            HiddenNode::new(ConstructorExpressionNode {
                constructor: String::from("True"),
            }),
        );

        let false_node = visitor.node(
            span,
            HiddenNode::new(ConstructorExpressionNode {
                constructor: String::from("False"),
            }),
        );

        visitor.visit(&true_node);
        visitor.visit(&false_node);

        visitor.constraint(GroupConstraint::new(node.clone(), true_node.clone()));
        visitor.constraint(GroupConstraint::new(node.clone(), false_node.clone()));

        visitor.insert(
            node,
            ResolvedIs {
                true_variant: true_node,
                false_variant: false_node,
            },
        );
    }
}

impl Codegen for IsExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let ResolvedIs {
            true_variant,
            false_variant,
        } = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.codegen(&self.left)?;

        ctx.push_conditions();
        ctx.codegen(&self.right)?;
        let conditions = ctx.pop_conditions();

        ctx.push_instructions();
        ctx.codegen(&true_variant)?;
        let true_instructions = ctx.pop_instructions();

        ctx.push_instructions();
        ctx.codegen(&false_variant)?;
        let false_instructions = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::If {
            node: Some(node.clone()),
            branches: vec![(conditions, true_instructions, Some(true_variant.clone()))],
            else_branch: Some((false_instructions, Some(false_variant.clone()))),
        });

        Ok(())
    }
}
