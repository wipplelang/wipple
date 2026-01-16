use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{ExpressionStatementNode, parse_comments, parse_statements, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{Type, TypeConstraint},
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct BlockExpressionNode {
    pub statements: Vec<NodeRef>,
}

impl Node for BlockExpressionNode {}

pub fn parse_block_expression(parser: &mut Parser<'_>) -> Result<BlockExpressionNode, ParseError> {
    parser.token(TokenKind::LeftBrace)?;

    let statements = parse_statements(parser)?;
    let _ = parse_comments(parser)?;

    parser.token(TokenKind::RightBrace)?;

    Ok(BlockExpressionNode { statements })
}

impl Visit for BlockExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.push_scope();

        for statement in &self.statements {
            visitor.visit(statement);
            visitor.edge(statement, node, "statement");
        }

        visitor.pop_scope();

        let output = self
            .statements
            .last()
            .cloned()
            .map(Type::from)
            .unwrap_or_else(|| Type::from(visitor.unit_type()));

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.block_type(output),
        ));
    }
}

impl Codegen for BlockExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string("(async () => {");
        ctx.write_line();

        let should_append_unit = self
            .statements
            .last()
            .and_then(|statement| statement.downcast_ref::<ExpressionStatementNode>())
            .is_none();

        for (index, statement) in self.statements.iter().enumerate() {
            if !should_append_unit && index + 1 == self.statements.len() {
                ctx.write_string("return ");
            }

            ctx.write(statement)?;
            ctx.write_string(";");
            ctx.write_line();
        }

        if should_append_unit {
            ctx.write_string("return [];");
            ctx.write_line();
        }

        ctx.write_string("})");

        Ok(())
    }
}
