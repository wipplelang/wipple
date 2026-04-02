use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, Node, NodeRef, Render},
    nodes::{VariableExpressionNode, parse_atomic_expression, visit_expression},
    syntax::{ParseError, Parser},
    typecheck::TypeConstraint,
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct ResolvedCall {
    pub is_unit: bool,
}

impl Fact for ResolvedCall {}

impl Render for ResolvedCall {}

#[derive(Debug)]
pub struct CallExpressionNode {
    pub function: NodeRef,
    pub inputs: Vec<NodeRef>,
}

impl Node for CallExpressionNode {}

pub fn parse_call_expression(parser: &mut Parser<'_>) -> Result<CallExpressionNode, ParseError> {
    let function = parse_atomic_expression(parser)?;

    let inputs = parser
        .parse_many(1, parse_atomic_expression, |parser| parser.parse_nothing())?
        .into_iter()
        .map(|(node, _)| node)
        .collect();

    Ok(CallExpressionNode { function, inputs })
}

impl Visit for CallExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        if self.inputs.len() == 1
            && let Some(variable) = self.inputs[0].downcast_ref::<VariableExpressionNode>()
        {
            let unit_constant = visitor
                .peek(&variable.variable, |definition| match definition {
                    Definition::Constant(definition) => Some(definition.clone()),
                    _ => None,
                })
                .find(|definition| definition.attributes.unit);

            if unit_constant.is_some() {
                let input = &self.inputs[0];

                visitor.visit(input);
                visitor.visit(&self.function);

                visitor.constraint(TypeConstraint::new(
                    input.clone(),
                    visitor
                        .db
                        .function_type([self.function.clone()], node.clone()),
                ));

                visitor.edge(&self.function, node, "function");
                visitor.edge(input, node, "input");

                visitor.insert(node, ResolvedCall { is_unit: true });

                return;
            }
        }

        visitor.visit(&self.function);
        visitor.edge(&self.function, node, "function");

        for input in &self.inputs {
            visitor.visit(input);
            visitor.edge(input, node, "input");
        }

        visitor.constraint(TypeConstraint::new(
            self.function.clone(),
            visitor
                .db
                .function_type(self.inputs.iter().cloned(), node.clone()),
        ));

        visitor.insert(node, ResolvedCall { is_unit: false });
    }
}

impl Codegen for CallExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let ResolvedCall { is_unit } = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        if is_unit {
            ctx.codegen(&self.inputs[0])?;

            ctx.codegen(&self.function)?;

            ctx.instruction(ir::Instruction::Value {
                node: node.clone(),
                value: ir::Value::Call {
                    function: self.inputs[0].clone(),
                    inputs: vec![self.function.clone()],
                },
            });
        } else {
            ctx.codegen(&self.function)?;

            for input in &self.inputs {
                ctx.codegen(input)?;
            }

            ctx.instruction(ir::Instruction::Value {
                node: node.clone(),
                value: ir::Value::Call {
                    function: self.function.clone(),
                    inputs: self.inputs.clone(),
                },
            });
        }

        Ok(())
    }
}
