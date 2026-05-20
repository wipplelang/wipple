use crate::{
    expressions::{operator_expression::parse_operator_expression, visit_expression},
    types::parse_type_element,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
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
pub struct AnnotateExpression {
    pub span: Span,
    pub expression: Box<dyn Visit>,
    pub ty: Box<dyn Visit>,
}

pub fn parse_annotate_expression(parser: &mut Parser) -> Result<AnnotateExpression, ParseError> {
    let span = parser.spanned();
    let expression = parse_operator_expression(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let ty = parse_type_element(parser)?;
    Ok(AnnotateExpression {
        span: span(parser),
        expression,
        ty,
    })
}

#[typetag::serde]
impl Visit for AnnotateExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let expression = visitor.visit(db, self.expression);
        let ty = visitor.visit(db, self.ty);
        db.graph.edge(ty, expression, "type");

        visitor.constraint(db, GroupConstraint::new(expression, ty));
        visitor.constraint(db, GroupConstraint::new(node, expression));

        visitor.codegen(db, node, AnnotateExpressionCodegen { node, expression });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnnotateExpressionCodegen {
    node: Node,
    expression: Node,
}

#[typetag::serde]
impl CodegenValue for AnnotateExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.expression)?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Variable(self.expression),
        });

        Ok(())
    }
}
