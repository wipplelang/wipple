pub mod bound_constraint;
pub mod default_constraint;

use crate::constraints::{
    bound_constraint::parse_bound_constraint, default_constraint::parse_default_constraint,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    visit::Visitor,
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

pub fn parse_constraints(parser: &mut Parser<'_>) -> Result<Vec<AstKey>, ParseError> {
    parser.token(TokenKind::WhereKeyword)?;
    parser.commit("in these constraints");

    parser.parse_many(0, parse_constraint)
}

pub fn parse_constraint(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_bound_constraint as value => parser.in_ast(value),
        parse_default_constraint as value => parser.in_ast(value),
        _ => "Expected a constraint",
    })
}

pub fn visit_constraint(db: &mut Db, node: Node, _visitor: &mut Visitor) {
    db.insert(node, IsConstraint);
}
