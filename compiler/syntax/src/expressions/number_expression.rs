use crate::{expressions::visit_expression, types::named_type::NamedType};

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
pub struct NumberExpression {
    pub span: Span,
    pub value: Substr,
}

pub fn parse_number_expression(parser: &mut Parser) -> Result<NumberExpression, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::Number)?;
    Ok(NumberExpression {
        span: span(parser),
        value,
    })
}

#[typetag::serde]
impl Visit for NumberExpression {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let number_type = db.node();
        db.hide(number_type);
        let syntax = Box::new(NamedType {
            span: self.span.clone(),
            name: Substr::from("Number"),
            parameters: Vec::new(),
        }) as Box<dyn Visit>;
        visitor.visit_as(db, syntax, number_type);

        visitor.constraint(db, GroupConstraint::new(node, number_type));
        visitor.codegen(
            db,
            node,
            NumberExpressionCodegen {
                node,
                value: self.value.clone(),
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct NumberExpressionCodegen {
    node: Node,
    value: Substr,
}

#[typetag::serde]
impl CodegenValue for NumberExpressionCodegen {
    fn codegen(&self, _db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Number(self.value.to_string()),
        });

        Ok(())
    }
}
