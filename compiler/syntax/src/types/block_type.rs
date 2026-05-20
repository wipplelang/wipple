use crate::types::{parse_type_element, visit_type};

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
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockType {
    pub span: Span,
    pub output: Box<dyn Visit>,
}

pub fn parse_block_type(parser: &mut Parser) -> Result<BlockType, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LeftBrace)?;
    let output = parse_type_element(parser)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(BlockType {
        span: span(parser),
        output,
    })
}

#[typetag::serde]
impl Visit for BlockType {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_type(db, node, visitor);

        let output = visitor.visit(db, self.output);
        db.graph.edge(output, node, "output");

        visitor.constraint(
            db,
            TyConstraint::new(node, ConstructedTy::block(Ty::Node(output))),
        );
    }
}
