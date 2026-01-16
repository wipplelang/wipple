mod annotate;
mod r#as;
mod block;
mod call;
mod collection;
mod constructor;
mod r#do;
mod format;
mod function;
mod intrinsic;
mod r#is;
mod number;
mod operator;
mod placeholder;
mod string;
mod structure;
mod tuple;
mod unit;
mod variable;
mod r#when;

pub use annotate::*;
pub use r#as::*;
pub use block::*;
pub use call::*;
pub use collection::*;
pub use constructor::*;
pub use r#do::*;
pub use format::*;
pub use function::*;
pub use intrinsic::*;
pub use r#is::*;
pub use number::*;
pub use operator::*;
pub use placeholder::*;
pub use string::*;
pub use structure::*;
pub use tuple::*;
pub use unit::*;
pub use variable::*;
pub use r#when::*;

use crate::{
    database::{Db, Fact, NodeRef, Render},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::Typed,
    visit::Visitor,
};

#[derive(Debug, Clone)]
pub struct IsExpression;

impl Fact for IsExpression {}

impl Render for IsExpression {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is an expression")
    }
}

pub fn parse_expression(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_function_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_tuple_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_empty_collection_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_collection_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_is_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_as_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_annotate_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.parse_optional(parse_operator_expression)? {
            return Ok(node);
        }

        parse_expression_element(parser)
    })
}

pub fn parse_expression_element(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_format_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_structure_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_call_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_do_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_when_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_intrinsic_expression)? {
            return Ok(node);
        }

        parse_atomic_expression(parser)
    })
}

pub fn parse_atomic_expression(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(node) = parser.try_node(parse_placeholder_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_variable_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_constructor_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_number_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_string_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_block_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.try_node(parse_unit_expression)? {
            return Ok(node);
        }

        if let Some(node) = parser.parse_optional(parse_parenthesized_expression)? {
            return Ok(node);
        }

        Err(parser.error("Expected expression"))
    })
}

pub fn parse_parenthesized_expression(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;

    parser.consume_line_breaks();
    let value = parse_expression(parser)?;
    parser.consume_line_breaks();

    parser.token(TokenKind::RightParenthesis)?;

    Ok(value)
}

pub fn visit_expression(node: &NodeRef, visitor: &mut Visitor<'_>) {
    visitor.insert(node, IsExpression);
    visitor.insert(node, Typed::default());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test_parse;

    #[test]
    fn test_parse_variable_expression() {
        test_parse("parse_variable_expression", parse_expression, r#"foo"#);
    }

    #[test]
    fn test_parse_number_expression() {
        test_parse("parse_number_expression", parse_expression, r#"3.14"#);
    }

    #[test]
    fn test_parse_string_expression() {
        test_parse("parse_string_expression", parse_expression, r#""abc""#);
    }

    #[test]
    fn test_parse_format_expression() {
        test_parse(
            "parse_format_expression",
            parse_expression,
            r#""Hello, _!" name"#,
        );
    }

    #[test]
    fn test_parse_structure_expression() {
        test_parse(
            "parse_structure_expression",
            parse_expression,
            r#"
            Foo {
                a : b
                c : d
            }
        "#,
        );
    }

    #[test]
    fn test_parse_empty_block_expression() {
        test_parse("parse_empty_block_expression", parse_expression, r#"{}"#);
    }

    #[test]
    fn test_parse_block_expression() {
        test_parse("parse_block_expression", parse_expression, r#"{foo}"#);
    }

    #[test]
    fn test_parse_do_expression() {
        test_parse("parse_do_expression", parse_expression, r#"do foo"#);
    }

    #[test]
    fn test_parse_simple_intrinsic_expression() {
        test_parse(
            "parse_simple_intrinsic_expression",
            parse_expression,
            r#"intrinsic "message""#,
        );
    }

    #[test]
    fn test_parse_complex_intrinsic_expression() {
        test_parse(
            "parse_complex_intrinsic_expression",
            parse_expression,
            r#"intrinsic "message" x y"#,
        );
    }

    #[test]
    fn test_parse_when_expression() {
        test_parse(
            "parse_when_expression",
            parse_expression,
            r#"
            when x {
                a -> b
                c -> d
            }
        "#,
        );
    }

    #[test]
    fn test_parse_call_expression() {
        test_parse("parse_call_expression", parse_expression, r#"f x y"#);
    }

    #[test]
    fn test_parse_annotate_expression() {
        test_parse(
            "parse_annotate_expression",
            parse_expression,
            r#"(3.14 :: Number)"#,
        );
    }

    #[test]
    fn test_parse_as_expression() {
        test_parse("parse_as_expression", parse_expression, r#"x as T"#);
    }

    #[test]
    fn test_parse_empty_collection_expression() {
        test_parse(
            "parse_empty_collection_expression",
            parse_expression,
            r#"(,)"#,
        );
    }

    #[test]
    fn test_parse_single_element_collection_expression() {
        test_parse(
            "parse_single_element_collection_expression",
            parse_expression,
            r#"a,"#,
        );
    }

    #[test]
    fn test_parse_single_line_collection_expression() {
        test_parse(
            "parse_single_line_collection_expression",
            parse_expression,
            r#"a, b, c"#,
        );
    }

    #[test]
    fn test_parse_multiline_collection_expression() {
        test_parse(
            "parse_multiline_collection_expression",
            parse_expression,
            r#"
            (
                a,
                b,
                c,
            )
        "#,
        );
    }

    #[test]
    fn test_parse_single_input_function_expression() {
        test_parse(
            "parse_single_input_function_expression",
            parse_expression,
            r#"x -> y"#,
        );
    }

    #[test]
    fn test_parse_multi_input_function_expression() {
        test_parse(
            "parse_multi_input_function_expression",
            parse_expression,
            r#"x y -> z"#,
        );
    }

    #[test]
    fn test_parse_complex_input_function_expression() {
        test_parse(
            "parse_complex_input_function_expression",
            parse_expression,
            r#"(X y) -> z"#,
        );
    }

    #[test]
    fn test_parse_simple_is_expression() {
        test_parse(
            "parse_simple_is_expression",
            parse_expression,
            r#"x is None"#,
        );
    }

    #[test]
    fn test_parse_complex_is_expression() {
        test_parse(
            "parse_complex_is_expression",
            parse_expression,
            r#"x is Some 3.14"#,
        );
    }
}
