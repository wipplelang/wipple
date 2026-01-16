mod assignment;
mod constant_definition;
mod expression;
mod instance_definition;
mod trait_definition;
mod type_definition;

pub use assignment::*;
pub use constant_definition::*;
pub use expression::*;
pub use instance_definition::*;
pub use trait_definition::*;
pub use type_definition::*;

use crate::{
    database::NodeRef,
    syntax::{ParseError, Parser, TokenKind},
};

pub fn parse_statements(parser: &mut Parser<'_>) -> Result<Vec<NodeRef>, ParseError> {
    parser.parse_lines(0, true, |parser| {
        let statement = parse_statement(parser)?;

        let _ = parser.parse_optional(parse_comment)?;

        Ok(statement)
    })
}

pub fn parse_statement(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(move |parser| {
        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_type_definition_statement))?
        {
            return Ok(node);
        }

        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_trait_definition_statement))?
        {
            return Ok(node);
        }

        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_constant_definition_statement))?
        {
            return Ok(node);
        }

        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_instance_definition_statement))?
        {
            return Ok(node);
        }

        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_assignment_statement))?
        {
            return Ok(node);
        }

        if let Some(node) =
            parser.parse_optional(|parser| parser.node(parse_expression_statement))?
        {
            return Ok(node);
        }

        Err(parser.error("Expected statement"))
    })
}

pub fn parse_comments(parser: &mut Parser<'_>) -> Result<Vec<String>, ParseError> {
    parser.parse_lines(0, true, parse_comment)
}

pub fn parse_comment(parser: &mut Parser<'_>) -> Result<String, ParseError> {
    parser.token(TokenKind::Comment)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_type_definition() {
        test_parse(
            "parse_type_definition",
            parse_statement,
            r#"
            -- Documentation comment
            [foo]
            Foo : type
        "#,
        );
    }

    #[test]
    fn test_parse_generic_type_definition() {
        test_parse(
            "parse_generic_type_definition",
            parse_statement,
            r#"Foo : value => type"#,
        );
    }

    #[test]
    fn test_parse_marker_type_definition() {
        test_parse(
            "parse_marker_type_definition",
            parse_statement,
            r#"Foo : type"#,
        );
    }

    #[test]
    fn test_parse_structure_type_definition() {
        test_parse(
            "parse_structure_type_definition",
            parse_statement,
            r#"
            Foo : type {
                a :: A
                b :: B
            }
        "#,
        );
    }

    #[test]
    fn test_parse_enumeration_type_definition() {
        test_parse(
            "parse_enumeration_type_definition",
            parse_statement,
            r#"
            Foo : type {
                Some Number
                None
            }
        "#,
        );
    }

    #[test]
    fn test_parse_trait_definition() {
        test_parse(
            "parse_trait_definition",
            parse_statement,
            r#"Foo : trait Number"#,
        );
    }

    #[test]
    fn test_parse_generic_trait_definition() {
        test_parse(
            "parse_generic_trait_definition",
            parse_statement,
            r#"Foo : value => trait (value -> Number)"#,
        );
    }

    #[test]
    fn test_parse_constant_definition() {
        test_parse(
            "parse_constant_definition",
            parse_statement,
            r#"show :: value -> Unit where (Show value)"#,
        );
    }

    #[test]
    fn test_parse_simple_valued_instance_definition() {
        test_parse(
            "parse_simple_valued_instance_definition",
            parse_statement,
            r#"instance (Foo Number) : 3.14"#,
        );
    }

    #[test]
    fn test_parse_complex_valued_instance_definition() {
        test_parse(
            "parse_complex_valued_instance_definition",
            parse_statement,
            r#"instance (Foo (Maybe value)) where (Foo value) : 3.14"#,
        );
    }

    #[test]
    fn test_parse_assignment() {
        test_parse("parse_assignment", parse_statement, r#"x : 123"#);
    }

    #[test]
    fn test_parse_expression_statement() {
        test_parse("parse_expression_statement", parse_statement, r#"123"#);
    }
}
