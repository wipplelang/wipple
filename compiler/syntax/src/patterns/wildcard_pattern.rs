use crate::patterns::visit_pattern;
use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue},
    db::{Db, Node},
    span::Span,
    visit::{Visit, Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WildcardPattern {
    pub span: Span,
}

pub fn parse_wildcard_pattern(parser: &mut Parser) -> Result<WildcardPattern, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::UnderscoreKeyword)?;
    parser.commit("in this wildcard pattern");
    Ok(WildcardPattern { span: span(parser) })
}

#[typetag::serde]
impl Visit for WildcardPattern {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, Some(MatchPathSegment::Match));

        visitor.codegen(db, node, WildcardPatternCodegen);
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct WildcardPatternCodegen;

#[typetag::serde]
impl CodegenValue for WildcardPatternCodegen {
    fn codegen(&self, _db: &Db, _ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        Ok(())
    }
}
