use crate::types::visit_type;
use serde::{Deserialize, Serialize};
use wipple_core::{
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
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitType {
    pub span: Span,
}

pub fn parse_unit_type(parser: &mut Parser<'_>) -> Result<UnitType, ParseError> {
    let span = parser.spanned();
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.token(TokenKind::RightParenthesis)?;
    Ok(UnitType { span: span(parser) })
}

#[typetag::serde]
impl Visit for UnitType {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        visitor.constraint(
            db,
            TyConstraint::new(node, Ty::Constructed(ConstructedTy::unit())),
        );
    }
}
