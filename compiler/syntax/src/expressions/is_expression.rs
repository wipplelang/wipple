use crate::{
    expressions::{
        constructor_expression::ConstructorExpression, parse_expression_element, visit_expression,
    },
    patterns::parse_pattern_element,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    arcstr::Substr,
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
pub struct IsExpression {
    pub span: Span,
    pub left: Box<dyn Visit>,
    pub right: Box<dyn Visit>,
}

pub fn parse_is_expression(parser: &mut Parser) -> Result<IsExpression, ParseError> {
    let span = parser.spanned();
    let left = parse_expression_element(parser)?;
    parser.token(TokenKind::IsOperator)?;
    parser.consume_line_breaks();
    let right = parse_pattern_element(parser)?;
    Ok(IsExpression {
        span: span(parser),
        left,
        right,
    })
}

#[typetag::serde]
impl Visit for IsExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let left = visitor.visit(db, self.left);
        db.graph.edge(left, node, "left");

        let right = self.right;
        let right = visitor.matching(
            left,
            |current_match| {
                current_match.allow_or = true;
                current_match.root = Some(left);
            },
            |visitor| {
                let right = visitor.visit(db, right);
                db.graph.edge(right, node, "right");
                right
            },
        );

        let true_node = Box::new(ConstructorExpression {
            span: self.span.clone(),
            constructor: Substr::from("True"),
        }) as Box<dyn Visit>;
        let true_node = visitor.visit(db, true_node);

        let false_node = Box::new(ConstructorExpression {
            span: self.span.clone(),
            constructor: Substr::from("False"),
        }) as Box<dyn Visit>;
        let false_node = visitor.visit(db, false_node);

        visitor.constraint(db, GroupConstraint::new(node, true_node));
        visitor.constraint(db, GroupConstraint::new(node, false_node));

        visitor.codegen(
            db,
            node,
            IsExpressionCodegen {
                node,
                left,
                right,
                true_node,
                false_node,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct IsExpressionCodegen {
    node: Node,
    left: Node,
    right: Node,
    true_node: Node,
    false_node: Node,
}

#[typetag::serde]
impl CodegenValue for IsExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.left)?;

        ctx.push_conditions();
        ctx.codegen(db, self.right)?;
        let conditions = ctx.pop_conditions();

        ctx.push_instructions();
        ctx.codegen(db, self.true_node)?;
        let true_instructions = ctx.pop_instructions();

        ctx.push_instructions();
        ctx.codegen(db, self.false_node)?;
        let false_instructions = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::If {
            node: Some(self.node),
            branches: vec![(conditions, true_instructions, Some(self.true_node))],
            else_branch: Some((false_instructions, Some(self.false_node))),
        });

        Ok(())
    }
}
