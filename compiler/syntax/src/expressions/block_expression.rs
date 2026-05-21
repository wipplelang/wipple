use crate::{
    expressions::visit_expression,
    statements::{parse_comments, parse_statements},
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    anyhow,
    ast::AstKey,
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    facts::Syntax,
    span::Span,
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        ty::{ConstructedTy, Ty},
    },
    visit::{Captures, Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockExpression {
    pub span: Span,
    pub statements: Vec<AstKey>,
}

pub fn parse_block_expression(parser: &mut Parser<'_>) -> Result<BlockExpression, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LeftBrace)?;
    let statements = parse_statements(parser)?;
    let _ = parse_comments(parser)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(BlockExpression {
        span: span(parser),
        statements,
    })
}

#[typetag::serde]
impl Visit for BlockExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        let statements = visitor.push_scope_with_statements(db, node, self.statements);

        let statements = statements
            .into_iter()
            .map(|(statement_node, statement)| {
                visitor.visit_as(db, &statement, statement_node);
                db.graph.edge(statement_node, node, "statement");
                statement_node
            })
            .collect::<Vec<_>>();

        visitor.pop_scope(db);

        visitor.constraint(
            db,
            TyConstraint::new(
                node,
                ConstructedTy::block(
                    statements
                        .last()
                        .copied()
                        .map(Ty::Node)
                        .unwrap_or_else(|| Ty::Constructed(ConstructedTy::unit())),
                ),
            ),
        );

        visitor.codegen(db, node, BlockExpressionCodegen { node, statements });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct BlockExpressionCodegen {
    node: Node,
    statements: Vec<Node>,
}

#[typetag::serde]
impl CodegenValue for BlockExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let captures = db
            .get::<Captures>(self.node)
            .map(|captures| captures.0.iter().copied().collect())
            .unwrap_or_default();

        ctx.push_instructions();

        if self.statements.is_empty() {
            ctx.instruction(ir::Instruction::Value {
                node: self.node,
                value: ir::Value::Tuple(Vec::new()),
            });

            ctx.instruction(ir::Instruction::Return { value: self.node });
        } else {
            for (index, &statement) in self.statements.iter().enumerate() {
                let span = db
                    .get(statement)
                    .map(|Syntax(syntax)| db.ast(syntax).span(db).clone())
                    .ok_or_else(|| anyhow::format_err!("missing span"))?;

                ctx.instruction(ir::Instruction::Trace { span });

                ctx.codegen(db, statement)?;

                if index + 1 == self.statements.len() {
                    ctx.instruction(ir::Instruction::Return { value: statement });
                }
            }
        }

        let instructions = ctx.pop_instructions();
        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Function {
                inputs: Vec::new(),
                captures,
                instructions,
            },
        });

        Ok(())
    }
}
