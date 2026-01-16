use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{
        ConstructorExpressionNode, ResolvedWhen, WhenArm, WhenExpressionNode, WildcardPatternNode,
        parse_expression_element, parse_pattern_element, visit_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedIs {
    pub input_temporary: NodeRef,
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

        let input_span = visitor.span(&self.left);
        let input_temporary = visitor.node(input_span, HiddenNode(None));

        visitor.matching(input_temporary.clone(), false, |visitor| {
            visitor.visit(&self.right);
            visitor.edge(&self.right, node, "right");
        });

        visitor.constraint(GroupConstraint::new(
            input_temporary.clone(),
            self.left.clone(),
        ));

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
                input_temporary,
                true_variant: true_node,
                false_variant: false_node,
            },
        );
    }
}

impl Codegen for IsExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let ResolvedIs {
            input_temporary,
            true_variant,
            false_variant,
        } = ctx
            .db
            .get::<ResolvedIs>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        let span = ctx.db.span(ctx.current_node());

        let wildcard_node = ctx.db.node(span.clone(), WildcardPatternNode);

        let when_expression = ctx.db.node(
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

        ctx.db
            .insert(&when_expression, ResolvedWhen { input_temporary });

        ctx.write(&when_expression)?;

        Ok(())
    }
}
