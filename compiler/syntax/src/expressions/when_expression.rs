use crate::{
    expressions::{parse_atomic_expression, parse_expression, visit_expression},
    patterns::parse_atomic_pattern,
};

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
pub struct WhenExpression {
    pub span: Span,
    pub input: AstKey,
    pub arms: Vec<WhenArm>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WhenArm {
    pub span: Span,
    pub pattern: AstKey,
    pub value: AstKey,
}

pub fn parse_when_expression(parser: &mut Parser<'_>) -> Result<WhenExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::WhenKeyword)?;
    parser.commit("in this `when` expression");
    let input = parse_atomic_expression(parser)?;
    parser.token(TokenKind::LeftBrace)?;
    let arms = parse_arms(parser)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(WhenExpression {
        span: span(parser),
        input,
        arms,
    })
}

pub fn parse_arm(parser: &mut Parser<'_>) -> Result<WhenArm, ParseError> {
    let span = parser.spanned();
    let pattern = parse_atomic_pattern(parser)?;
    parser.commit("in this `when` arm");
    parser.token(TokenKind::FunctionOperator)?;
    parser.consume_line_breaks();
    let value = parse_expression(parser)?;
    Ok(WhenArm {
        span: span(parser),
        pattern,
        value,
    })
}

pub fn parse_arms(parser: &mut Parser<'_>) -> Result<Vec<WhenArm>, ParseError> {
    parser.parse_lines(0, true, parse_arm)
}

#[typetag::serde]
impl Visit for WhenArm {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for WhenExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let input = visitor.visit(db, &self.input);
        db.graph.edge(input, node, "input");

        let arms = self
            .arms
            .into_iter()
            .map(|arm| {
                visitor.matching(
                    input,
                    |current_match| {
                        current_match.allow_or = true;
                        current_match.allow_set = false;
                        current_match.root = Some(input);
                    },
                    |visitor| {
                        let pattern = db.node();
                        visitor.current_match.as_mut().unwrap().arm = Some(pattern);

                        visitor.push_scope(db, pattern);

                        visitor.visit_as(db, &arm.pattern, pattern);
                        db.graph.edge(pattern, node, "pattern");

                        let value = visitor.visit(db, &arm.value);
                        db.graph.edge(value, node, "value");

                        visitor.pop_scope(db);

                        visitor.constraint(db, GroupConstraint::new(value, node));

                        (pattern, value)
                    },
                )
            })
            .collect::<Vec<_>>();

        visitor.codegen(db, node, WhenExpressionCodegen { node, input, arms });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct WhenExpressionCodegen {
    node: Node,
    input: Node,
    arms: Vec<(Node, Node)>,
}

#[typetag::serde]
impl CodegenValue for WhenExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.input)?;

        let branches = self
            .arms
            .iter()
            .map(|(pattern, value)| {
                ctx.push_conditions();
                ctx.codegen(db, *pattern)?;
                let conditions = ctx.pop_conditions();

                ctx.push_instructions();
                ctx.codegen(db, *value)?;
                let instructions = ctx.pop_instructions();

                Ok((conditions, instructions, Some(*value)))
            })
            .collect::<Result<Vec<_>, CodegenError>>()?;

        ctx.instruction(ir::Instruction::If {
            node: Some(self.node),
            branches,
            else_branch: None,
        });

        Ok(())
    }
}
