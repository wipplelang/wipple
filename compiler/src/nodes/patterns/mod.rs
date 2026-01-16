mod annotate;
mod constructor;
mod number;
mod or;
mod set;
mod string;
mod structure;
mod tuple;
mod unit;
mod variable;
mod wildcard;

pub use annotate::*;
pub use constructor::*;
pub use number::*;
pub use or::*;
pub use set::*;
pub use string::*;
pub use structure::*;
pub use tuple::*;
pub use unit::*;
pub use variable::*;
pub use wildcard::*;

use crate::{
    database::{Db, Fact, NodeRef, Render},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{GroupConstraint, Typed},
    visit::Visitor,
};
use std::collections::BTreeSet;

#[derive(Debug, Clone)]
pub struct IsPattern;

impl Fact for IsPattern {}

impl Render for IsPattern {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is a pattern")
    }
}

#[derive(Debug, Clone)]
pub struct Matching(pub NodeRef);

impl Fact for Matching {}

impl Render for Matching {}

pub fn parse_pattern(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_tuple_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_or_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_annotate_pattern)? {
            return Ok(node);
        }

        parse_pattern_element(parser)
    })
}

pub fn parse_pattern_element(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_structure_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_parameterized_constructor_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_set_pattern)? {
            return Ok(node);
        }

        parse_atomic_pattern(parser)
    })
}

pub fn parse_atomic_pattern(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_constructor_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_wildcard_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_variable_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_number_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_string_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_unit_pattern)? {
            return Ok(node);
        }

        if let Some(node) = parser.parse_optional(parse_parenthesized_pattern)? {
            return Ok(node);
        }

        Err(parser.error("Expected pattern"))
    })
}

pub fn parse_parenthesized_pattern(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;

    parser.consume_line_breaks();
    let value = parse_pattern(parser)?;
    parser.consume_line_breaks();

    parser.token(TokenKind::RightParenthesis)?;

    Ok(value)
}

pub fn visit_pattern(node: &NodeRef, visitor: &mut Visitor<'_>) {
    visitor.insert(node, IsPattern);
    visitor.insert(node, Typed::default());

    let matching = visitor.current_match().node.clone();
    visitor.insert(node, Matching(matching.clone()));
    visitor.edge(&matching, node, "value");
    visitor.constraint(GroupConstraint::new(node.clone(), matching));
}

#[derive(Debug, Clone)]
pub struct InheritTemporaries(pub Vec<NodeRef>);

impl Fact for InheritTemporaries {}

impl Render for InheritTemporaries {}

#[derive(Debug, Clone)]
pub struct HasTemporaries(pub Vec<NodeRef>);

impl Fact for HasTemporaries {}

impl Render for HasTemporaries {}

impl Db {
    pub fn temporaries(&self, node: &NodeRef) -> BTreeSet<NodeRef> {
        self.get::<InheritTemporaries>(node)
            .map(|InheritTemporaries(temporaries)| temporaries)
            .unwrap_or_default()
            .into_iter()
            .flat_map(|node| self.temporaries(&node))
            .chain(
                self.get::<HasTemporaries>(node)
                    .map(|HasTemporaries(temporaries)| temporaries)
                    .unwrap_or_default(),
            )
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_wildcard_pattern() {
        test_parse("parse_wildcard_pattern", parse_pattern, r#"_"#);
    }

    #[test]
    fn test_parse_variable_pattern() {
        test_parse("parse_variable_pattern", parse_pattern, r#"x"#);
    }

    #[test]
    fn test_parse_structure_pattern() {
        test_parse("parse_structure_pattern", parse_pattern, r#"Foo {x : y}"#);
    }

    #[test]
    fn test_parse_set_pattern() {
        test_parse("parse_set_pattern", parse_pattern, r#"set x"#);
    }

    #[test]
    fn test_parse_simple_constructor_pattern() {
        test_parse("parse_simple_constructor_pattern", parse_pattern, r#"None"#);
    }

    #[test]
    fn test_parse_complex_constructor_pattern() {
        test_parse(
            "parse_complex_constructor_pattern",
            parse_pattern,
            r#"Some x y z"#,
        );
    }

    #[test]
    fn test_parse_simple_or_pattern() {
        test_parse("parse_simple_or_pattern", parse_pattern, r#"x or y"#);
    }

    #[test]
    fn test_parse_complex_or_pattern() {
        test_parse("parse_complex_or_pattern", parse_pattern, r#"x or y or z"#);
    }
}
