use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{
        ConstructorExpressionNode, WhenArm, WhenExpressionNode, WildcardPatternNode,
        parse_expression_element, parse_pattern_element, visit_expression,
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

        let span = ctx.span(node);

        let wildcard_node = ctx.node(span.clone(), WildcardPatternNode);

        let when_expression = ctx.node(
            span,
            WhenExpressionNode {
                input: self.left.clone(),
                arms: vec![
                    WhenArm {
                        pattern: self.right.clone(),
                        value: true_variant,
                    },
                    WhenArm {
                        pattern: wildcard_node,
                        value: false_variant,
                    },
                ],
            },
        );

        ctx.codegen(&when_expression)?;

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Variable(when_expression),
        });

        Ok(())
    }
}
