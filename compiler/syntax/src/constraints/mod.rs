pub mod bound_constraint;
pub mod default_constraint;

use crate::constraints::{
    bound_constraint::parse_bound_constraint, default_constraint::parse_default_constraint,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parse_alt,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsConstraint;

#[typetag::serde]
impl Fact for IsConstraint {}

impl Render for IsConstraint {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is a constraint");
    }
}

pub fn parse_constraints(parser: &mut Parser) -> Result<Vec<Box<dyn Visit>>, ParseError> {
    parser.token(TokenKind::WhereKeyword)?;
    parser.commit("in these constraints");

    parser.parse_many(0, parse_constraint)
}

pub fn parse_constraint(parser: &mut Parser) -> Result<Box<dyn Visit>, ParseError> {
    parse_alt!(parser, {
        parse_bound_constraint as constraint => Box::new(constraint),
        parse_default_constraint as constraint => Box::new(constraint),
        _ => "Expected a constraint",
    })
}

pub fn visit_constraint(db: &mut Db, node: Node, _visitor: &mut Visitor) {
    db.insert(node, IsConstraint);
}
