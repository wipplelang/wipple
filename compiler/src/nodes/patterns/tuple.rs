use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{HasTemporaries, InheritTemporaries, Matching, parse_pattern_element, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug)]
pub struct TuplePatternNode {
    pub elements: Vec<NodeRef>,
}

impl Node for TuplePatternNode {}

pub fn parse_tuple_pattern(parser: &mut Parser<'_>) -> Result<TuplePatternNode, ParseError> {
    let elements = parser
        .parse_many(1, parse_pattern_element, |parser| {
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

    Ok(TuplePatternNode { elements })
}

impl Visit for TuplePatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

        let element_temporaries = self
            .elements
            .iter()
            .map(|element| {
                let temporary = visitor.visit_matching(element);
                visitor.edge(element, node, "element");
                temporary
            })
            .collect::<Vec<_>>();

        visitor.insert(node, HasTemporaries(element_temporaries.clone()));

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.tuple_type(element_temporaries),
        ));

        visitor.insert(node, InheritTemporaries(self.elements.clone()));
    }
}

impl Codegen for TuplePatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let HasTemporaries(element_temporaries) = ctx
            .db
            .get::<HasTemporaries>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        let Matching(matching) = ctx
            .db
            .get::<Matching>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        for (index, (element, temporary)) in
            self.elements.iter().zip(element_temporaries).enumerate()
        {
            ctx.write_string(" && ((");
            ctx.write_node(&temporary);
            ctx.write_string(" = ");
            ctx.write_node(&matching);
            ctx.write_string("[");
            ctx.write_string(index.to_string());
            ctx.write_string("]) || true)");

            ctx.write(element)?;
        }

        Ok(())
    }
}
