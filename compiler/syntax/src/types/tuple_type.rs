use crate::types::{parse_type_element, visit_type};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TupleType {
    pub span: Span,
    pub elements: Vec<AstKey>,
}

pub fn parse_tuple_type(parser: &mut Parser<'_>) -> Result<TupleType, ParseError> {
    let span = parser.spanned();
    let elements = parser
        .parse_sep(1, parse_type_element, |parser| {
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

    Ok(TupleType {
        span: span(parser),
        elements,
    })
}

#[typetag::serde]
impl Visit for TupleType {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        let elements = self
            .elements
            .into_iter()
            .map(|element| visitor.visit(db, &element))
            .collect::<Vec<_>>();

        for &element in &elements {
            db.graph.edge(element, node, "element");
        }

        visitor.constraint(
            db,
            TyConstraint::new(
                node,
                ConstructedTy::tuple(elements.into_iter().map(Ty::Node).collect()),
            ),
        );
    }
}
