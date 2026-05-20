use crate::{
    expressions::{
        constructor_expression::ConstructorExpression, parse_expression_element, visit_expression,
    },
    types::parse_type_element,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    arcstr::Substr,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::{group_constraint::GroupConstraint, ty_constraint::TyConstraint},
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AsExpression {
    pub span: Span,
    pub left: Box<dyn Visit>,
    pub right: Box<dyn Visit>,
}

pub fn parse_as_expression(parser: &mut Parser) -> Result<AsExpression, ParseError> {
    let span = parser.spanned();
    let left = parse_expression_element(parser)?;
    parser.token(TokenKind::AsOperator)?;
    parser.consume_line_breaks();
    let right = parse_type_element(parser)?;
    Ok(AsExpression {
        span: span(parser),
        left,
        right,
    })
}

#[typetag::serde]
impl Visit for AsExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let left = visitor.visit(db, self.left);
        db.graph.edge(left, node, "left");

        let right = visitor.visit(db, self.right);
        db.graph.edge(right, node, "right");

        let as_function = Box::new(ConstructorExpression {
            span: self.span.clone(),
            constructor: Substr::from("As"),
        }) as Box<dyn Visit>;
        let as_function = visitor.visit(db, as_function);

        visitor.constraint(
            db,
            TyConstraint::new(
                as_function,
                ConstructedTy::function(vec![Ty::Node(left)], Ty::Node(right)),
            ),
        );

        visitor.constraint(db, GroupConstraint::new(node, right));

        visitor.codegen(
            db,
            node,
            AsExpressionCodegen {
                node,
                as_function,
                left,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AsExpressionCodegen {
    node: Node,
    as_function: Node,
    left: Node,
}

#[typetag::serde]
impl CodegenValue for AsExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.as_function)?;
        ctx.codegen(db, self.left)?;
        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Call {
                function: self.as_function,
                inputs: vec![self.left],
            },
        });
        Ok(())
    }
}
