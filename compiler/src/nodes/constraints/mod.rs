mod bound;
mod default;

pub use bound::*;
pub use default::*;

use crate::{
    database::{Db, Fact, NodeRef, Render},
    syntax::{ParseError, Parser, TokenKind},
    visit::Visitor,
};

#[derive(Debug, Clone)]
pub struct IsConstraint;

impl Fact for IsConstraint {}

impl Render for IsConstraint {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is a constraint")
    }
}

pub fn parse_constraints(parser: &mut Parser<'_>) -> Result<Vec<NodeRef>, ParseError> {
    parser.token(TokenKind::WhereKeyword)?;
    parser.commit("in these constraints");

    let constraints = parser
        .parse_many(0, parse_constraint, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    Ok(constraints)
}

pub fn parse_constraint(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_bound_constraint)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_default_constraint)? {
            return Ok(node);
        }

        Err(parser.error("Expected constraint"))
    })
}

pub fn visit_constraint(node: &NodeRef, visitor: &mut Visitor<'_>) {
    visitor.insert(node, IsConstraint);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_bound_constraint() {
        test_parse("parse_bound_constraint", parse_constraint, r#"(Foo value)"#);
    }

    #[test]
    fn test_parse_default_constraint() {
        test_parse(
            "parse_default_constraint",
            parse_constraint,
            r#"(value :: Number)"#,
        );
    }
}
