pub mod annotate_expression;
pub mod as_expression;
pub mod block_expression;
pub mod call_expression;
pub mod collection_expression;
pub mod constructor_expression;
pub mod do_expression;
pub mod format_expression;
pub mod function_expression;
pub mod intrinsic_expression;
pub mod is_expression;
pub mod number_expression;
pub mod operator_expression;
pub mod placeholder_expression;
pub mod string_expression;
pub mod structure_expression;
pub mod tuple_expression;
pub mod unit_expression;
pub mod variable_expression;
pub mod when_expression;

use serde::{Deserialize, Serialize};

use crate::expressions::{
    annotate_expression::parse_annotate_expression,
    as_expression::parse_as_expression,
    block_expression::parse_block_expression,
    call_expression::parse_call_expression,
    collection_expression::{parse_collection_expression, parse_empty_collection_expression},
    constructor_expression::parse_constructor_expression,
    do_expression::parse_do_expression,
    format_expression::parse_format_expression,
    function_expression::parse_function_expression,
    intrinsic_expression::parse_intrinsic_expression,
    is_expression::parse_is_expression,
    number_expression::parse_number_expression,
    operator_expression::{parse_operator_expression, parse_parenthesized_operator_expression},
    placeholder_expression::parse_placeholder_expression,
    string_expression::parse_string_expression,
    structure_expression::parse_structure_expression,
    tuple_expression::parse_tuple_expression,
    unit_expression::parse_unit_expression,
    variable_expression::parse_variable_expression,
    when_expression::parse_when_expression,
};
use wipple_core::{
    ast::AstKey,
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::groups::Typed,
    visit::Visitor,
};
use wipple_parse::{
    lexer::TokenKind,
    parse_alt,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsExpression;

#[typetag::serde]
impl Fact for IsExpression {}

impl Render for IsExpression {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is an expression");
    }
}

pub fn parse_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_function_expression as value => parser.in_ast(value),
        parse_tuple_expression as value => parser.in_ast(value),
        parse_empty_collection_expression as value => parser.in_ast(value),
        parse_collection_expression as value => parser.in_ast(value),
        parse_is_expression as value => parser.in_ast(value),
        parse_as_expression as value => parser.in_ast(value),
        parse_annotate_expression as value => parser.in_ast(value),
        parse_operator_expression as value => value,
        parse_expression_element as value => value,
        _ => "Expected expression",
    })
}

pub fn parse_expression_element(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_format_expression as value => parser.in_ast(value),
        parse_structure_expression as value => parser.in_ast(value),
        parse_call_expression as value => parser.in_ast(value),
        parse_do_expression as value => parser.in_ast(value),
        parse_when_expression as value => parser.in_ast(value),
        parse_intrinsic_expression as value => parser.in_ast(value),
        parse_atomic_expression as value => value,
        _ => "Expected expression",
    })
}

pub fn parse_atomic_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_placeholder_expression as value => parser.in_ast(value),
        parse_variable_expression as value => parser.in_ast(value),
        parse_constructor_expression as value => parser.in_ast(value),
        parse_number_expression as value => parser.in_ast(value),
        parse_string_expression as value => parser.in_ast(value),
        parse_block_expression as value => parser.in_ast(value),
        parse_unit_expression as value => parser.in_ast(value),
        parse_parenthesized_operator_expression as value => parser.in_ast(value),
        parse_parenthesized_expression as value => value,
        _ => "Expected expression",
    })
}

pub fn parse_parenthesized_expression(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.consume_line_breaks();
    let value = parse_expression(parser)?;
    parser.consume_line_breaks();
    parser.token(TokenKind::RightParenthesis)?;
    Ok(value)
}

pub fn visit_expression(db: &mut Db, node: Node, _visitor: &mut Visitor) {
    db.insert(node, IsExpression);
    db.insert(node, Typed::default());
}
