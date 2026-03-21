use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::{Matching, Temporaries, parse_pattern_element, visit_pattern},
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

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.tuple_type(element_temporaries.clone()),
        ));

        visitor.insert(
            node,
            Temporaries {
                has: element_temporaries,
                inherit: self.elements.clone(),
            },
        )
    }
}

impl Codegen for TuplePatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Temporaries {
            has: element_temporaries,
            ..
        } = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        for (index, (element, temporary)) in
            self.elements.iter().zip(element_temporaries).enumerate()
        {
            ctx.condition(ir::Condition::Initialize {
                variable: temporary,
                node: None,
                value: ir::Value::TupleElement {
                    input: matching.clone(),
                    index,
                },
                mutable: false,
            });

            ctx.codegen(element)?;
        }

        Ok(())
    }
}
