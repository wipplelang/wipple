use crate::expressions::visit_expression;
use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{constraints::ty_constraint::TyConstraint, ty::ConstructedTy},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, ParseToken, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnitExpression {
    pub span: Span,
}

pub fn parse_unit_expression(parser: &mut Parser) -> Result<UnitExpression, ParseError> {
    let span = parser.spanned();
    parser
        .token(ParseToken::from(TokenKind::LeftParenthesis).reason("between these parentheses"))?;
    parser.token(TokenKind::RightParenthesis)?;
    Ok(UnitExpression { span: span(parser) })
}

#[typetag::serde]
impl Visit for UnitExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        visitor.constraint(db, TyConstraint::new(node, ConstructedTy::unit()));
        visitor.codegen(db, node, UnitExpressionCodegen { node });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UnitExpressionCodegen {
    node: Node,
}

#[typetag::serde]
impl CodegenValue for UnitExpressionCodegen {
    fn codegen(&self, _db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Tuple(Vec::new()),
        });

        Ok(())
    }
}
