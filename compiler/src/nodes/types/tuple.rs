use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{parse_type_element, visit_type},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct TupleTypeNode {
    pub elements: Vec<NodeRef>,
}

impl Node for TupleTypeNode {}

pub fn parse_tuple_type(parser: &mut Parser<'_>) -> Result<TupleTypeNode, ParseError> {
    let elements = parser
        .parse_many(1, parse_type_element, |parser| {
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

    Ok(TupleTypeNode { elements })
}

impl Visit for TupleTypeNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

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

impl Codegen for TupleTypeNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Err(ctx.error())
    }
}
