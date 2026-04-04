use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::{parse_comments, parse_expression},
    syntax::{ParseError, Parser},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct ExpressionStatementNode {
    pub expression: NodeRef,
}

impl Node for ExpressionStatementNode {
    fn is_hidden(&self) -> bool {
        true
    }
}

pub fn parse_expression_statement(
    parser: &mut Parser<'_>,
) -> Result<ExpressionStatementNode, ParseError> {
    let _ = parse_comments(parser)?;
    let expression = parse_expression(parser)?;

    Ok(ExpressionStatementNode { expression })
}

impl Visit for ExpressionStatementNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        let expression = self.expression.clone();
        let node = node.clone();

        visitor.after_all_definitions(move |visitor| {
            visitor.visit(&expression);
            visitor.graph.replace(&node, &expression);
            visitor.constraint(GroupConstraint::new(node.clone(), expression));
        });
    }
}

impl Codegen for ExpressionStatementNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        ctx.codegen(&self.expression)?;

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Variable(self.expression.clone()),
        });

        Ok(())
    }
}
