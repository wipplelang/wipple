use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Node, NodeRef},
    nodes::{parse_atomic_pattern, parse_expression, visit_expression},
    syntax::{ParseError, Parser, TokenKind},
    typecheck::TypeConstraint,
    visit::{Captures, Visit, Visitor},
};

#[derive(Debug)]
pub struct FunctionExpressionNode {
    pub inputs: Vec<NodeRef>,
    pub output: NodeRef,
}

impl Node for FunctionExpressionNode {}

pub fn parse_function_expression(
    parser: &mut Parser<'_>,
) -> Result<FunctionExpressionNode, ParseError> {
    let inputs = parse_function_expression_inputs(parser)?;
    let output = parse_expression(parser)?;

    Ok(FunctionExpressionNode { inputs, output })
}

pub fn parse_function_expression_inputs(
    parser: &mut Parser<'_>,
) -> Result<Vec<NodeRef>, ParseError> {
    let inputs = parser
        .parse_many(1, parse_atomic_pattern, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    parser.token(TokenKind::FunctionOperator)?;
    parser.commit("in this function");
    parser.consume_line_breaks();

    Ok(inputs)
}

impl Visit for FunctionExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        visitor.push_scope();

        for pattern in &self.inputs {
            visitor.matching(pattern, false, false, |visitor| {
                visitor.current_match().root = Some(pattern.clone());
                visitor.current_match().arm = Some(pattern.clone());
                visitor.visit(pattern);
            });

            visitor.edge(pattern, node, "input");
        }

        visitor.visit(&self.output);
        visitor.edge(&self.output, node, "output");

        visitor.pop_scope();

        visitor.constraint(TypeConstraint::new(
            node.clone(),
            visitor.function_type(self.inputs.iter().cloned(), self.output.clone()),
        ));
    }
}

impl Codegen for FunctionExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Captures(captures) = ctx.get(node).unwrap_or_default();

        ctx.push_instructions();

        ctx.push_conditions();
        for pattern in &self.inputs {
            ctx.codegen(pattern)?;
        }
        let conditions = ctx.pop_conditions();

        ctx.instruction(ir::Instruction::If {
            node: None,
            branches: vec![(conditions, Vec::new(), None)],
            else_branch: None,
        });

        ctx.codegen(&self.output)?;

        ctx.instruction(ir::Instruction::Return {
            value: self.output.clone(),
        });

        let instructions = ctx.pop_instructions();

        ctx.instruction(ir::Instruction::Value {
            node: node.clone(),
            value: ir::Value::Function {
                inputs: self.inputs.clone(),
                captures: Vec::from_iter(captures),
                instructions,
            },
        });

        Ok(())
    }
}
