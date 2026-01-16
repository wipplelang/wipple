use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_expression_element, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct TupleExpressionNode {
    pub elements: Vec<NodeRef>,
}

impl Node for TupleExpressionNode {}

pub fn parse_tuple_expression(parser: &mut Parser<'_>) -> Result<TupleExpressionNode, ParseError> {
    let elements = parser
        .parse_many(1, parse_expression_element, |parser| {
            parser.token(TokenKind::TupleOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    if elements.len() == 1 {
        parser.token(TokenKind::TupleOperator)?;
    } else {
        let _ = parser.parse_optional(|parser| {
            parser.token(TokenKind::TupleOperator)?;
            Ok(())
        })?;
    }

    Ok(TupleExpressionNode { elements })
}

impl Visit for TupleExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        for element in &self.elements {
            visitor.visit(element);
            visitor.edge(element, node, "element");
        }

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.tuple_type(self.elements.iter().cloned()),
        ));
    }
}

impl Codegen for TupleExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string("[");
        for element in &self.elements {
            ctx.write(element)?;
            ctx.write_string(", ");
        }
        ctx.write_string("]");

        Ok(())
    }
}
