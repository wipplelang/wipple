use crate::expressions::{parse_expression_element, visit_expression};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::Span,
    typecheck::{constraints::ty_constraint::TyConstraint, ty::ConstructedTy},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TupleExpression {
    pub span: Span,
    pub elements: Vec<AstKey>,
}

pub fn parse_tuple_expression(parser: &mut Parser<'_>) -> Result<TupleExpression, ParseError> {
    let span = parser.spanned();
    let elements = parser
        .parse_sep(1, parse_expression_element, |parser| {
            parser.token(TokenKind::TupleOperator)?;
            parser.consume_line_breaks();
            Ok(())
        })?
        .into_iter()
        .map(|(element, _)| element)
        .collect::<Vec<_>>();

    if elements.len() == 1 {
        parser.token(TokenKind::TupleOperator)?;
    } else {
        let _ = parser.parse_optional(|parser| parser.token(TokenKind::TupleOperator))?;
    }

    Ok(TupleExpression {
        span: span(parser),
        elements,
    })
}

#[typetag::serde]
impl Visit for TupleExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let elements = self
            .elements
            .into_iter()
            .map(|element| visitor.visit(db, &element))
            .collect::<Vec<_>>();

        for &element in &elements {
            db.graph.edge(element, node, "element");
        }

        visitor.constraint(
            db,
            TyConstraint::new(node, ConstructedTy::tuple(elements.clone())),
        );

        visitor.codegen(db, node, TupleExpressionCodegen { node, elements });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TupleExpressionCodegen {
    node: Node,
    elements: Vec<Node>,
}

#[typetag::serde]
impl CodegenValue for TupleExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        for &element in &self.elements {
            ctx.codegen(db, element)?;
        }

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Tuple(self.elements.clone()),
        });

        Ok(())
    }
}
