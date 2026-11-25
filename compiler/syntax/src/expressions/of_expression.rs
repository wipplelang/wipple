use crate::expressions::{parse_expression_element, visit_expression};
use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenError, hir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{constraints::field_constraint::FieldConstraint, resolved::ResolvedField},
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    names::parse_variable_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OfExpression {
    pub span: Span,
    pub left: Str,
    pub right: AstKey,
}

pub fn parse_of_expression(parser: &mut Parser<'_>) -> Result<OfExpression, ParseError> {
    let span = parser.spanned();
    let left = parse_variable_name(parser)?;
    parser.token(TokenKind::OfOperator)?;
    parser.consume_line_breaks();
    let right = parse_expression_element(parser)?;
    Ok(OfExpression {
        span: span(parser),
        left,
        right,
    })
}

#[typetag::serde]
impl Visit for OfExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let right = visitor.visit(db, &self.right);
        if let Some(graph) = &mut db.graph {
            graph.edge(right, node, "value");
        }

        visitor.constraint(db, FieldConstraint::new(node, right, self.left.clone()));

        visitor.codegen(
            db,
            node,
            OfExpressionCodegen {
                node,
                field: self.left.clone(),
                value: right,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OfExpressionCodegen {
    node: Node,
    field: Str,
    value: Node,
}

#[typetag::serde]
impl hir::Write for OfExpressionCodegen {
    fn write(&self, db: &Db, ctx: &mut hir::Ctx) -> Result<(), CodegenError> {
        let field_index = db
            .get(self.node)
            .and_then(|ResolvedField { index, .. }| index.as_ref().ok().copied())
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        ctx.write(db, self.value)?;

        ctx.push_conditions();
        ctx.condition(hir::Condition::Field {
            input: self.value,
            field_name: self.field.to_string(),
            field_index,
            variable: self.node,
        });
        let conditions = ctx.pop_conditions();

        ctx.instruction(hir::Instruction::If {
            node: Some(self.node),
            branches: vec![(conditions, Vec::new(), None)],
            else_branch: None,
        });

        Ok(())
    }
}
