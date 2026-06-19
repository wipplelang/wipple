use crate::{
    expressions::parse_expression,
    statements::{parse_comments, visit_statement},
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{constraints::ty_constraint::TyConstraint, groups::Typed, ty::Ty},
    visit::{Visit, Visitor},
};
use wipple_parse::parser::{ParseError, Parser};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: AstKey,
}

pub fn parse_expression_statement(
    parser: &mut Parser<'_>,
) -> Result<ExpressionStatement, ParseError> {
    let span = parser.spanned();
    let _ = parse_comments(parser)?;
    let expression = parse_expression(parser)?;
    Ok(ExpressionStatement {
        span: span(parser),
        expression,
    })
}

#[typetag::serde]
impl Visit for ExpressionStatement {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn is_hidden(&self, _db: &Db) -> bool {
        true
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_statement(db, node, visitor);

        db.insert(node, Typed::default());

        let expression = visitor.visit(db, &self.expression);

        db.graph.replace(node, expression);

        visitor.constraint(db, TyConstraint::new(node, Ty::Node(expression)));

        visitor.codegen(db, node, ExpressionStatementCodegen { node, expression });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ExpressionStatementCodegen {
    node: Node,
    expression: Node,
}

#[typetag::serde]
impl CodegenValue for ExpressionStatementCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.codegen(db, self.expression)?;

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Variable(self.expression),
        });

        Ok(())
    }
}
