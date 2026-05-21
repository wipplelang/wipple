use crate::{expressions::visit_expression, types::named_type::NamedType};

use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
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
    pub value: Str,
}

pub fn parse_number_expression(parser: &mut Parser<'_>) -> Result<NumberExpression, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::Number)?;
    Ok(NumberExpression {
        span: span(parser),
        value,
    })
}

#[typetag::serde]
impl Visit for NumberExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let number_type = db.node();
        db.hide(number_type);
        let syntax = visitor.in_ast(
            db,
            Box::new(NamedType {
                span: self.span.clone(),
                name: Str::from("Number"),
                parameters: Vec::new(),
            }),
        );
        visitor.visit_as(db, &syntax, number_type);

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
    value: Str,
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
