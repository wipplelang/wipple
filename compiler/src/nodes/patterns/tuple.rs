use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Node, NodeRef},
    nodes::{HasTemporaries, InheritTemporaries, Matching, parse_pattern_element, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{MatchPathSegment, Visit, Visitor},
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
        visit_pattern(node, visitor, None);

        let element_temporaries = self
            .elements
            .iter()
            .enumerate()
            .map(|(index, element)| {
                let temporary = visitor.visit_matching(
                    element,
                    Some(MatchPathSegment::TupleElement(index, self.elements.len())),
                );

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
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let HasTemporaries(element_temporaries) = ctx.get::<HasTemporaries>(node)?;
        let Matching(matching) = ctx.get(node)?;

        let mut expressions = Vec::new();
        for (index, (element, temporary)) in
            self.elements.iter().zip(element_temporaries).enumerate()
        {
            expressions.push(
                ir::Expression::AssignTo(
                    Box::new(
                        ir::Expression::Index(
                            Box::new(ir::Expression::Variable(matching.clone()).at(node, ctx)),
                            index,
                        )
                        .at(node, ctx),
                    ),
                    temporary,
                )
                .at(node, ctx),
            );
            expressions.push(ctx.codegen(element)?);
        }

        Some(ir::Expression::And(expressions).at(node, ctx))
    }
}
