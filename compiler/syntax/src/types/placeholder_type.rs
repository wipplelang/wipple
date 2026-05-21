use crate::types::visit_type;
use serde::{Deserialize, Serialize};
use wipple_core::{
    db::{Db, Node},
    span::Span,
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlaceholderType {
    pub span: Span,
}

pub fn parse_placeholder_type(parser: &mut Parser<'_>) -> Result<PlaceholderType, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::UnderscoreKeyword)?;
    Ok(PlaceholderType { span: span(parser) })
}

#[typetag::serde]
impl Visit for PlaceholderType {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);
    }
}
