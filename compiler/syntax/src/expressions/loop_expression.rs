use crate::{
    expressions::{parse_atomic_expression, visit_expression},
    types::named_type::NamedType,
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{constraints::ty_constraint::TyConstraint, ty::Ty},
    visit::{Visit, VisitAs, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoopExpression {
    pub span: Span,
    pub body: AstKey,
}

pub fn parse_loop_expression(parser: &mut Parser<'_>) -> Result<LoopExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LoopKeyword)?;
    let body = parse_atomic_expression(parser)?;
    Ok(LoopExpression {
        span: span(parser),
        body,
    })
}

#[typetag::serde]
impl Visit for LoopExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let body = visitor.visit(db, &self.body);

        let result_syntax = visitor.in_ast(db, Box::new(self.span.clone()));

        let result = db.node();
        db.hide(result);

        let control_flow_parameter = visitor.in_ast(
            db,
            Box::new(VisitAs {
                node: result,
                syntax: result_syntax,
            }),
        );

        let control_flow_syntax = visitor.in_ast(
            db,
            Box::new(NamedType {
                span: self.span.clone(),
                name: Str::from("Control-Flow"),
                parameters: vec![control_flow_parameter],
            }),
        );
        let control_flow_type = visitor.visit(db, &control_flow_syntax);
        db.hide(control_flow_type);

        visitor.constraint(db, TyConstraint::new(body, Ty::Node(control_flow_type)));
        visitor.constraint(db, TyConstraint::new(node, Ty::Node(result)));

        visitor.codegen(db, node, LoopExpressionCodegen { node, body });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct LoopExpressionCodegen {
    node: Node,
    body: Node,
}

#[typetag::serde]
impl CodegenValue for LoopExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.push_instructions();
        ctx.codegen(db, self.body)?;
        let body = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::Loop {
            node: self.node,
            body,
            result: self.body,
        });

        Ok(())
    }
}
