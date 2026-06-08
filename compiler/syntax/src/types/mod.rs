pub mod block_type;
pub mod function_type;
pub mod named_type;
pub mod placeholder_type;
pub mod tuple_type;
pub mod type_parameter;
pub mod unit_type;

use serde::{Deserialize, Serialize};

use crate::types::{
    block_type::parse_block_type,
    function_type::parse_function_type,
    named_type::{parse_named_type, parse_parameterized_type},
    placeholder_type::parse_placeholder_type,
    tuple_type::parse_tuple_type,
    type_parameter::{parse_annotated_type_parameter, parse_type_parameter},
    unit_type::parse_unit_type,
};

use wipple_core::{
    ast::AstKey,
    db::{Db, Fact, Node},
    facts::GraphType,
    render::{Render, RenderCtx},
    typecheck::groups::{Annotated, Typed},
    visit::Visitor,
};
use wipple_parse::{
    lexer::TokenKind,
    parse_alt,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsType;

#[typetag::serde]
impl Fact for IsType {}

impl Render for IsType {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is a type");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingTypes(pub Vec<Node>);

#[typetag::serde]
impl Fact for MissingTypes {}

impl Render for MissingTypes {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("missing types");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraType;

#[typetag::serde]
impl Fact for ExtraType {}

impl Render for ExtraType {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is extra type");
    }
}

pub fn parse_type(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_tuple_type as value => parser.in_ast(value),
        parse_function_type as value => parser.in_ast(value),
        parse_annotated_type_parameter as value => parser.in_ast(value),
        parse_type_element as value => value,
        _ => "Expected type",
    })
}

pub fn parse_type_element(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_parameterized_type as value => parser.in_ast(value),
        parse_atomic_type as value => value,
        _ => "Expected type",
    })
}

pub fn parse_atomic_type(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_placeholder_type as value => parser.in_ast(value),
        parse_type_parameter as value => value,
        parse_named_type as value => parser.in_ast(value),
        parse_block_type as value => parser.in_ast(value),
        parse_unit_type as value => parser.in_ast(value),
        parse_parenthesized_type as value => value,
        _ => "Expected type",
    })
}

pub fn parse_parenthesized_type(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.consume_line_breaks();
    let value = parse_type(parser)?;
    parser.consume_line_breaks();
    parser.token(TokenKind::RightParenthesis)?;
    Ok(value)
}

pub fn visit_type(db: &mut Db, node: Node, _visitor: &mut Visitor) {
    db.insert(node, IsType);
    db.insert(node, GraphType);
    db.insert(node, Typed::default());
    db.insert(node, Annotated);
}
