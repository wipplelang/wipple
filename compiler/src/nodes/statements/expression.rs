use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
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
            visitor.edge(&expression, &node, "expression");
            visitor.constraint(GroupConstraint::new(node.clone(), expression));
        });
    }
}

impl Codegen for ExpressionStatementNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write(&self.expression)?;
        ctx.write_string(";");
        ctx.write_line();
        Ok(())
    }
}
