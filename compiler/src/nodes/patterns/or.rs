use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{InheritTemporaries, parse_pattern_element, visit_pattern},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::GroupConstraint,
    visit::{Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct InvalidOrPattern;

impl Fact for InvalidOrPattern {}

impl Render for InvalidOrPattern {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is invalid `or` pattern")
    }
}

#[derive(Debug)]
pub struct OrPatternNode {
    pub patterns: Vec<NodeRef>,
}

impl Node for OrPatternNode {}

pub fn parse_or_pattern(parser: &mut Parser<'_>) -> Result<OrPatternNode, ParseError> {
    let patterns = parser
        .parse_many(2, parse_pattern_element, |parser| {
            parser.token(TokenKind::OrOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    Ok(OrPatternNode { patterns })
}

impl Visit for OrPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor, None);

        if !visitor.current_match().allow_or {
            visitor.insert(node, InvalidOrPattern);
        }

        let prev_arm = visitor.current_match().arm.take();

        for pattern in &self.patterns {
            if prev_arm.is_some() {
                // HACK: This only works because `or` is only supported at the
                // top level, effectively making each pattern its own arm
                visitor.current_match().arm = Some(pattern.clone());
            } else {
                // Not checking for exhaustiveness here (e.g. inside an `is` expression)
            }

            visitor.visit(pattern);
            visitor.edge(pattern, node, "pattern");
            visitor.constraint(GroupConstraint::new(node.clone(), pattern.clone()));
        }

        visitor.current_match().arm = prev_arm;

        visitor.insert(node, InheritTemporaries(self.patterns.clone()));
    }
}

impl Codegen for OrPatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        ctx.write_string(" && (false");

        for pattern in &self.patterns {
            ctx.write_string(" || (true");
            ctx.write(pattern)?;
            ctx.write_string(")");
        }

        ctx.write_string(")");

        Ok(())
    }
}
