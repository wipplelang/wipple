use crate::expressions::{parse_atomic_expression, visit_expression};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IntrinsicExpression {
    pub span: Span,
    pub name: Str,
    pub inputs: Vec<AstKey>,
}

pub fn parse_intrinsic_expression(
    parser: &mut Parser<'_>,
) -> Result<IntrinsicExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::IntrinsicKeyword)?;
    parser.commit("in this `intrinsic` expression");
    let name = parser.token(TokenKind::String)?;
    let inputs = parser.parse_many(0, parse_atomic_expression)?;
    Ok(IntrinsicExpression {
        span: span(parser),
        name,
        inputs,
    })
}

#[typetag::serde]
impl Visit for IntrinsicExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| {
                let input = visitor.visit(db, &input);
                db.graph.edge(input, node, "input");
                input
            })
            .collect::<Vec<_>>();

        visitor.codegen(
            db,
            node,
            IntrinsicExpressionCodegen {
                node,
                name: self.name.clone(),
                inputs,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct IntrinsicExpressionCodegen {
    node: Node,
    name: Str,
    inputs: Vec<Node>,
}

#[typetag::serde]
impl CodegenValue for IntrinsicExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        for &input in &self.inputs {
            ctx.codegen(db, input)?;
        }

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Runtime {
                name: self.name.to_string(),
                inputs: self.inputs.clone(),
            },
        });

        Ok(())
    }
}
