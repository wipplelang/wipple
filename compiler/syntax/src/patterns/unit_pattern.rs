use crate::patterns::visit_pattern;
use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue},
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitPattern {
    pub span: Span,
}

pub fn parse_unit_pattern(parser: &mut Parser<'_>) -> Result<UnitPattern, ParseError> {
    let span = parser.spanned();
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.token(TokenKind::RightParenthesis)?;
    Ok(UnitPattern { span: span(parser) })
}

#[typetag::serde]
impl Visit for UnitPattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::Match));

        visitor.constraint(
            db,
            TyConstraint::new(node, Ty::Constructed(ConstructedTy::unit())),
        );
        visitor.codegen(db, node, UnitPatternCodegen);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UnitPatternCodegen;

#[typetag::serde]
impl CodegenValue for UnitPatternCodegen {
    fn codegen(&self, _db: &Db, _ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        Ok(())
    }
}
