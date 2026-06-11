use crate::{expressions::visit_expression, types::named_type::NamedType};

use serde::{Deserialize, Serialize};
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    render::{Render, RenderCtx},
    span::{Span, Str},
    typecheck::{
        constraints::{ConstraintTrace, group_constraint::GroupConstraint},
        groups::Annotated,
        ty::Ty,
    },
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StringExpression {
    pub span: Span,
    pub value: Str,
}

pub fn parse_string_expression(parser: &mut Parser<'_>) -> Result<StringExpression, ParseError> {
    let span = parser.spanned();
    let value = parser.token(TokenKind::String)?;
    Ok(StringExpression {
        span: span(parser),
        value,
    })
}

#[typetag::serde]
impl Visit for StringExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let string_type = db.node();
        db.hide(string_type);
        let syntax = visitor.in_ast(
            db,
            Box::new(NamedType {
                span: self.span.clone(),
                name: Str::from("String"),
                parameters: Vec::new(),
            }),
        );
        visitor.visit_as(db, &syntax, string_type);

        visitor.constraint(
            db,
            GroupConstraint::new(node, string_type)
                .with_trace(StringConstraintTrace { node, string_type }),
        );

        db.insert(node, Annotated);

        visitor.codegen(
            db,
            node,
            StringExpressionCodegen {
                node,
                value: self.value.clone(),
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StringConstraintTrace {
    node: Node,
    string_type: Node,
}

#[typetag::serde]
impl ConstraintTrace for StringConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.node, &mut self.string_type]
    }

    fn nodes(&self, _db: &Db) -> Vec<Node> {
        vec![self.node, self.string_type]
    }
}

impl Render for StringConstraintTrace {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        ctx.node(self.node);
        ctx.string(" is a ");
        ctx.ty(db, &Ty::Node(self.string_type), self.string_type, true);
        ctx.string(".");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct StringExpressionCodegen {
    node: Node,
    value: Str,
}

#[typetag::serde]
impl CodegenValue for StringExpressionCodegen {
    fn codegen(&self, _db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::String(self.value.to_string()),
        });

        Ok(())
    }
}
