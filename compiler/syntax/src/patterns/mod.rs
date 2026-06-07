pub mod annotate_pattern;
pub mod constructor_pattern;
pub mod number_pattern;
pub mod or_pattern;
pub mod set_pattern;
pub mod string_pattern;
pub mod structure_pattern;
pub mod tuple_pattern;
pub mod unit_pattern;
pub mod variable_pattern;
pub mod wildcard_pattern;

use serde::{Deserialize, Serialize};

use crate::patterns::{
    annotate_pattern::parse_annotate_pattern,
    constructor_pattern::{parse_constructor_pattern, parse_parameterized_constructor_pattern},
    number_pattern::parse_number_pattern,
    or_pattern::parse_or_pattern,
    set_pattern::parse_set_pattern,
    string_pattern::parse_string_pattern,
    structure_pattern::parse_structure_pattern,
    tuple_pattern::parse_tuple_pattern,
    unit_pattern::parse_unit_pattern,
    variable_pattern::parse_variable_pattern,
    wildcard_pattern::parse_wildcard_pattern,
};
use std::collections::BTreeSet;
use wipple_core::{
    ast::AstKey,
    db::{Db, Fact, Node},
    render::{Render, RenderCtx},
    typecheck::{
        constraints::{ConstraintTrace, group_constraint::GroupConstraint},
        groups::Typed,
    },
    visit::{Visitor, exhaustiveness::MatchPathSegment},
};
use wipple_parse::{
    lexer::TokenKind,
    parse_alt,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Temporaries(pub BTreeSet<Node>);

#[typetag::serde]
impl Fact for Temporaries {}

impl Render for Temporaries {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraElement;

#[typetag::serde]
impl Fact for ExtraElement {}

impl Render for ExtraElement {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvalidOrPattern;

#[typetag::serde]
impl Fact for InvalidOrPattern {}

impl Render for InvalidOrPattern {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InvalidSetPattern {
    Nested,
    Immutable(Node),
}

#[typetag::serde]
impl Fact for InvalidSetPattern {}

impl Render for InvalidSetPattern {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IsPattern;

#[typetag::serde]
impl Fact for IsPattern {}

impl Render for IsPattern {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.string("is a pattern");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Matching(pub Node);

#[typetag::serde]
impl Fact for Matching {}

impl Render for Matching {}

pub fn parse_pattern(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_tuple_pattern as value => parser.in_ast(value),
        parse_or_pattern as value => parser.in_ast(value),
        parse_annotate_pattern as value => parser.in_ast(value),
        parse_pattern_element as value => value,
        _ => "Expected pattern",
    })
}

pub fn parse_pattern_element(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_structure_pattern as value => parser.in_ast(value),
        parse_parameterized_constructor_pattern as value => parser.in_ast(value),
        parse_set_pattern as value => parser.in_ast(value),
        parse_atomic_pattern as value => value,
        _ => "Expected pattern",
    })
}

pub fn parse_atomic_pattern(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_constructor_pattern as value => parser.in_ast(value),
        parse_wildcard_pattern as value => parser.in_ast(value),
        parse_variable_pattern as value => parser.in_ast(value),
        parse_number_pattern as value => parser.in_ast(value),
        parse_string_pattern as value => parser.in_ast(value),
        parse_unit_pattern as value => parser.in_ast(value),
        parse_parenthesized_pattern as value => value,
        _ => "Expected pattern",
    })
}

pub fn parse_parenthesized_pattern(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.consume_line_breaks();
    let value = parse_pattern(parser)?;
    parser.consume_line_breaks();
    parser.token(TokenKind::RightParenthesis)?;
    Ok(value)
}

pub fn visit_pattern(
    db: &mut Db,
    node: Node,
    visitor: &mut Visitor,
    terminal: Option<MatchPathSegment>,
) {
    db.insert(node, IsPattern);
    db.insert(node, Typed::default());

    let matching = visitor.set_matches(db, terminal);
    db.insert(node, Matching(matching));

    if visitor.current_match.as_ref().and_then(|m| m.arm) == Some(node) {
        db.graph.edge(matching, node, "value")
    }

    let mut constraint = GroupConstraint::new(node, matching);
    if node != matching {
        constraint = constraint.with_trace(MatchingConstraintTrace {
            pattern: node,
            value: matching,
        });
    }

    visitor.constraint(db, constraint);
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MatchingConstraintTrace {
    pattern: Node,
    value: Node,
}

#[typetag::serde]
impl ConstraintTrace for MatchingConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.pattern, &mut self.value]
    }

    fn nodes(&self, _db: &Db) -> Vec<Node> {
        vec![self.pattern, self.value]
    }

    fn allow_hidden_nodes(&self) -> bool {
        false
    }
}

impl Render for MatchingConstraintTrace {
    fn render_into(&self, _db: &Db, ctx: &mut RenderCtx) {
        ctx.node(self.pattern);
        ctx.string(" is ");
        ctx.node(self.value);
        ctx.string(".");
    }
}
