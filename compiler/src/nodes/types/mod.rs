mod block;
mod function;
mod named;
mod parameter;
mod placeholder;
mod tuple;
mod unit;

pub use block::*;
pub use function::*;
pub use named::*;
pub use parameter::*;
pub use placeholder::*;
pub use tuple::*;
pub use unit::*;

use crate::{
    database::{Db, Fact, NodeRef, Render},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::Typed,
    visit::Visitor,
};

#[derive(Debug, Clone)]
pub struct IsType;

impl Fact for IsType {}

impl Render for IsType {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is a type")
    }
}

#[derive(Debug, Clone)]
pub struct MissingTypes(pub Vec<NodeRef>);

impl Fact for MissingTypes {}

impl Render for MissingTypes {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing type")
    }
}

#[derive(Debug, Clone)]
pub struct ExtraType;

impl Fact for ExtraType {}

impl Render for ExtraType {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is extra type")
    }
}

pub fn parse_type(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_tuple_type)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_function_type)? {
            return Ok(node);
        }

        if let Some(node) = parser.parse_optional(parse_annotated_type_parameter_node)? {
            return Ok(node);
        }

        parse_type_element(parser)
    })
}

pub fn parse_type_element(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_parameterized_type)? {
            return Ok(node);
        }

        parse_atomic_type(parser)
    })
}

pub fn parse_atomic_type(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_placeholder_type)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_type_parameter)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_named_type)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_block)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_unit_type)? {
            return Ok(node);
        }

        if let Some(node) = parser.parse_optional(parse_parenthesized_type)? {
            return Ok(node);
        }

        Err(parser.error("Expected type"))
    })
}

pub fn parse_parenthesized_type(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;

    parser.consume_line_breaks();
    let value = parse_type(parser)?;
    parser.consume_line_breaks();

    parser.token(TokenKind::RightParenthesis)?;

    Ok(value)
}

pub fn visit_type(node: &NodeRef, visitor: &mut Visitor<'_>) {
    visitor.insert(node, IsType);
    visitor.insert(node, Typed::default());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_placeholder_type() {
        test_parse("parse_placeholder_type", parse_type, r#"_"#);
    }

    #[test]
    fn test_parse_unit_type() {
        test_parse("parse_unit_type", parse_type, r#"()"#);
    }

    #[test]
    fn test_parse_simple_named_type() {
        test_parse("parse_simple_named_type", parse_type, r#"Number"#);
    }

    #[test]
    fn test_parse_complex_named_type() {
        test_parse("parse_complex_named_type", parse_type, r#"Maybe Number"#);
    }

    #[test]
    fn test_parse_block_type() {
        test_parse("parse_block_type", parse_type, r#"{Number}"#);
    }

    #[test]
    fn test_parse_single_input_function_type() {
        test_parse(
            "parse_single_input_function_type",
            parse_type,
            r#"Number -> ()"#,
        );
    }

    #[test]
    fn test_parse_multi_input_function_type() {
        test_parse(
            "parse_multi_input_function_type",
            parse_type,
            r#"Number Number -> ()"#,
        );
    }

    #[test]
    fn test_parse_complex_input_function_type() {
        test_parse(
            "parse_complex_input_function_type",
            parse_type,
            r#"(Maybe Number) Number -> ()"#,
        );
    }
}
