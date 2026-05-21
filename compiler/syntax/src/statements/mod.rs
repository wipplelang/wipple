pub mod assignment_statement;
pub mod constant_definition;
pub mod expression_statement;
pub mod instance_definition;
pub mod trait_definition;
pub mod type_definition;

use crate::statements::{
    assignment_statement::parse_assignment_statement,
    constant_definition::parse_constant_definition_statement,
    expression_statement::parse_expression_statement,
    instance_definition::parse_instance_definition_statement,
    trait_definition::parse_trait_definition_statement,
    type_definition::parse_type_definition_statement,
};

use wipple_core::{ast::AstKey, span::Str};
use wipple_parse::{
    lexer::TokenKind,
    parse_alt,
    parser::{ParseError, Parser},
};

pub fn parse_statements(parser: &mut Parser<'_>) -> Result<Vec<AstKey>, ParseError> {
    parser.parse_lines(0, true, |parser| {
        let statement = parse_statement(parser)?;
        let _ = parser.parse_optional(parse_comment)?;
        Ok(statement)
    })
}

pub fn parse_statement(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_type_definition_statement as value => parser.in_ast(value),
        parse_trait_definition_statement as value => parser.in_ast(value),
        parse_constant_definition_statement as value => parser.in_ast(value),
        parse_instance_definition_statement as value => parser.in_ast(value),
        parse_assignment_statement as value => parser.in_ast(value),
        parse_expression_statement as value => parser.in_ast(value),
        _ => "Expected statement",
    })
}

pub fn parse_comments(parser: &mut Parser<'_>) -> Result<Vec<Str>, ParseError> {
    parser.parse_lines(0, true, parse_comment)
}

pub fn parse_comment(parser: &mut Parser<'_>) -> Result<Str, ParseError> {
    parser.token(TokenKind::Comment)
}
