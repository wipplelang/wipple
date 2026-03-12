use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, Node, NodeRef, Render},
    nodes::{
        ConstructorExpressionNode, parse_expression_element, parse_type_element, visit_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{GroupConstraint, TypeConstraint},
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct AsFunction(pub NodeRef);

impl Fact for AsFunction {}

impl Render for AsFunction {}

#[derive(Debug)]
pub struct AsExpressionNode {
    pub left: NodeRef,
    pub right: NodeRef,
}

impl Node for AsExpressionNode {}

pub fn parse_as_expression(parser: &mut Parser<'_>) -> Result<AsExpressionNode, ParseError> {
    let left = parse_expression_element(parser)?;
    parser.token(TokenKind::AsOperator)?;
    parser.consume_line_breaks();
    let right = parse_type_element(parser)?;

    Ok(AsExpressionNode { left, right })
}

impl Visit for AsExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.visit(&self.left);
        visitor.edge(&self.left, node, "left");

        visitor.visit(&self.right);
        visitor.edge(&self.right, node, "right");

        let span = visitor.span(node);
        let as_function = visitor.node(
            span,
            ConstructorExpressionNode {
                constructor: String::from("As"),
            },
        );

        visitor.visit(&as_function);
        visitor.constraint(TypeConstraint::new(
            as_function.clone(),
            visitor.function_type([self.left.clone()], self.right.clone()),
        ));

        visitor.constraint(GroupConstraint::new(node.clone(), self.right.clone()));

        visitor.insert(node, AsFunction(as_function));
    }
}

impl Codegen for AsExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let AsFunction(as_function) = ctx
            .get::<AsFunction>(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.codegen(&as_function)?;
        ctx.codegen(&self.left)?;

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Call {
                function: as_function,
                inputs: vec![self.left.clone()],
            },
        });

        Ok(())
    }
}
