use crate::patterns::{Matching, Temporaries, parse_pattern_element, visit_pattern};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{constraints::ty_constraint::TyConstraint, ty::ConstructedTy},
    visit::{Visit, Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TuplePattern {
    pub span: Span,
    pub elements: Vec<AstKey>,
}

pub fn parse_tuple_pattern(parser: &mut Parser<'_>) -> Result<TuplePattern, ParseError> {
    let span = parser.spanned();
    let elements = parser
        .parse_sep(1, parse_pattern_element, |parser| {
            parser.token(TokenKind::TupleOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(element, _)| element)
        .collect::<Vec<_>>();

    if elements.len() == 1 {
        parser.token(TokenKind::TupleOperator)?;
    } else {
        let _ = parser.parse_optional(|parser| parser.token(TokenKind::TupleOperator))?;
    }

    Ok(TuplePattern {
        span: span(parser),
        elements,
    })
}

#[typetag::serde]
impl Visit for TuplePattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, None);

        let element_count = self.elements.len();
        let elements = self
            .elements
            .into_iter()
            .enumerate()
            .map(|(index, element)| {
                let element = visitor.visit_matching(
                    db,
                    &element,
                    MatchPathSegment::TupleElement(index, element_count),
                );

                db.graph.edge(element.0, node, "element");

                element
            })
            .collect::<Vec<_>>();

        visitor.constraint(
            db,
            TyConstraint::new(
                node,
                ConstructedTy::tuple(elements.iter().map(|(_, temporary)| *temporary).collect()),
            ),
        );

        db.insert(
            node,
            Temporaries(
                elements
                    .iter()
                    .map(|(_, temporary)| *temporary)
                    .collect::<BTreeSet<_>>(),
            ),
        );

        visitor.codegen(db, node, TuplePatternCodegen { node, elements });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TuplePatternCodegen {
    node: Node,
    elements: Vec<(Node, Node)>,
}

#[typetag::serde]
impl CodegenValue for TuplePatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let matching = db
            .get::<Matching>(self.node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?
            .0;

        for (index, &(pattern, temporary)) in self.elements.iter().enumerate() {
            ctx.condition(ir::Condition::Initialize {
                variable: temporary,
                node: None,
                value: ir::Value::TupleElement {
                    input: matching,
                    index,
                },
                mutable: false,
            });

            ctx.codegen(db, pattern)?;
        }

        Ok(())
    }
}
