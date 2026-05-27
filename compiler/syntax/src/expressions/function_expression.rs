use crate::{
    expressions::{parse_expression, visit_expression},
    patterns::parse_atomic_pattern,
};

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
    visit::Captures,
    visit::{Visit, Visitor},
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExpression {
    pub span: Span,
    pub inputs: Vec<AstKey>,
    pub output: AstKey,
}

pub fn parse_function_expression(
    parser: &mut Parser<'_>,
) -> Result<FunctionExpression, ParseError> {
    let span = parser.spanned();
    let inputs = parse_function_expression_inputs(parser)?;
    let output = parse_expression(parser)?;
    Ok(FunctionExpression {
        span: span(parser),
        inputs,
        output,
    })
}

pub fn parse_function_expression_inputs(
    parser: &mut Parser<'_>,
) -> Result<Vec<AstKey>, ParseError> {
    let inputs = parser.parse_many(1, parse_atomic_pattern)?;
    parser.token(TokenKind::FunctionOperator)?;
    parser.commit("in this function");
    parser.consume_line_breaks();
    Ok(inputs)
}

#[typetag::serde]
impl Visit for FunctionExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        visitor.push_scope(db, node);

        let inputs = self
            .inputs
            .into_iter()
            .map(|input| {
                let input_node = db.node();

                visitor.matching(
                    input_node,
                    |current_match| {
                        current_match.mutable = false;
                        current_match.root = Some(input_node);
                    },
                    |visitor| visitor.visit_as(db, &input, input_node),
                );

                db.graph.edge(input_node, node, "input");
                input_node
            })
            .collect::<Vec<_>>();

        let output = visitor.visit(db, &self.output);
        db.graph.edge(output, node, "output");

        visitor.pop_scope(db);

        visitor.constraint(
            db,
            TyConstraint::new(
                node,
                ConstructedTy::function(
                    inputs.iter().copied().map(Ty::Node).collect(),
                    Ty::Node(output),
                ),
            ),
        );

        visitor.codegen(
            db,
            node,
            FunctionExpressionCodegen {
                node,
                inputs,
                output,
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct FunctionExpressionCodegen {
    node: Node,
    inputs: Vec<Node>,
    output: Node,
}

#[typetag::serde]
impl CodegenValue for FunctionExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        let captures = db
            .get::<Captures>(self.node)
            .map(|captures| captures.0.iter().copied().collect())
            .unwrap_or_default();

        ctx.push_instructions();
        ctx.push_conditions();

        for &input in &self.inputs {
            ctx.codegen(db, input)?;
        }

        let conditions = ctx.pop_conditions();

        ctx.instruction(ir::Instruction::If {
            node: None,
            branches: vec![(conditions, Vec::new(), None)],
            else_branch: None,
        });

        ctx.codegen(db, self.output)?;

        ctx.instruction(ir::Instruction::Return { value: self.output });

        let instructions = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::Value {
            node: self.node,
            value: ir::Value::Function(ir::Function {
                inputs: self.inputs.clone(),
                instructions,
                closure: Some((self.node, captures)),
            }),
        });

        Ok(())
    }
}
