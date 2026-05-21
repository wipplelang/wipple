use crate::patterns::{InvalidOrPattern, parse_pattern_element, visit_pattern};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::constraints::group_constraint::GroupConstraint,
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrPattern {
    pub span: Span,
    pub patterns: Vec<AstKey>,
}

pub fn parse_or_pattern(parser: &mut Parser<'_>) -> Result<OrPattern, ParseError> {
    let span = parser.spanned();
    let patterns = parser
        .parse_sep(2, parse_pattern_element, |parser| {
            parser.token(TokenKind::OrOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(pattern, _)| pattern)
        .collect();
    Ok(OrPattern {
        span: span(parser),
        patterns,
    })
}

#[typetag::serde]
impl Visit for OrPattern {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_pattern(db, node, visitor, None);

        if !visitor.current_match.as_ref().unwrap().allow_or {
            db.insert(node, InvalidOrPattern);
        }

        let previous_arm = visitor.current_match.as_ref().unwrap().arm;
        let patterns = self
            .patterns
            .into_iter()
            .map(|pattern| {
                let pattern_node = db.node();

                if previous_arm.is_some() {
                    visitor.current_match.as_mut().unwrap().arm = Some(pattern_node);
                }

                visitor.visit_as(db, &pattern, pattern_node);
                db.graph.edge(pattern_node, node, "pattern");
                visitor.constraint(db, GroupConstraint::new(node, pattern_node));

                pattern_node
            })
            .collect::<Vec<_>>();

        visitor.current_match.as_mut().unwrap().arm = previous_arm;

        visitor.codegen(db, node, OrPatternCodegen { patterns });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OrPatternCodegen {
    patterns: Vec<Node>,
}

#[typetag::serde]
impl CodegenValue for OrPatternCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let mut conditions = Vec::new();
        for &pattern in &self.patterns {
            ctx.push_conditions();
            ctx.codegen(db, pattern)?;
            conditions.push(ctx.pop_conditions());
        }

        ctx.condition(ir::Condition::Or(conditions));

        Ok(())
    }
}
