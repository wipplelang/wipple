use crate::expressions::{parse_atomic_expression, visit_expression};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoExpression {
    pub span: Span,
    pub input: AstKey,
}

pub fn parse_do_expression(parser: &mut Parser<'_>) -> Result<DoExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::DoKeyword)?;
    parser.commit("in this `do` expression");
    let input = parse_atomic_expression(parser)?;
    Ok(DoExpression {
        span: span(parser),
        input,
    })
}

#[typetag::serde]
impl Visit for DoExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let input = visitor.visit(db, &self.input);
        db.graph.edge(input, node, "input");

        visitor.constraint(
            db,
            TyConstraint::new(input, Ty::Constructed(ConstructedTy::block(node))),
        );
        visitor.codegen(db, node, DoExpressionCodegen { node, input });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DoExpressionCodegen {
    node: Node,
    input: Node,
}

#[typetag::serde]
impl CodegenValue for DoExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.input)?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Call {
                function: self.input,
                inputs: Vec::new(),
            },
        });

        Ok(())
    }
}
