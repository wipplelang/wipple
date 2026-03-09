use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{HasTemporaries, IsMutated, Matching, visit_pattern},
    syntax::{ParseError, Parser, TokenKind, parse_variable_name},
    typecheck::GroupConstraint,
    visit::{Definition, MatchPathSegment, Resolved, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct InvalidSetPattern;

impl Fact for InvalidSetPattern {}

impl Render for InvalidSetPattern {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is invalid `set` pattern")
    }
}

#[derive(Debug)]
pub struct SetPatternNode {
    pub variable: String,
}

impl Node for SetPatternNode {}

pub fn parse_set_pattern(parser: &mut Parser<'_>) -> Result<SetPatternNode, ParseError> {
    parser.token(TokenKind::SetKeyword)?;
    parser.commit("in this `set` pattern");
    let variable = parse_variable_name(parser)?;

    Ok(SetPatternNode { variable })
}

impl Visit for SetPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor, Some(MatchPathSegment::Match));

        if !visitor.current_match().allow_set {
            visitor.insert(node, InvalidSetPattern);
        }

        let Some(variable_definition) =
            visitor.resolve(&self.variable, node, |definition| match definition {
                Definition::Variable(definition) => Some(definition.clone()),
                _ => None,
            })
        else {
            return;
        };

        visitor.constraint(GroupConstraint::new(
            node.clone(),
            variable_definition.node.clone(),
        ));

        visitor.graph.replace(node, &variable_definition.node);

        // Do NOT include `self.matching_variable`, that would shadow it!
        visitor.insert(node, HasTemporaries(Vec::new()));

        visitor.insert(&variable_definition.node, IsMutated);
    }
}

impl Codegen for SetPatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let Resolved { definitions, .. } = ctx.get::<Resolved>(node)?;
        let matching_variable = definitions.into_iter().next()?;
        let Matching(matching) = ctx.get(node)?;

        Some(ir::Expression::Mutate(matching_variable, matching).at(node, ctx))
    }
}
