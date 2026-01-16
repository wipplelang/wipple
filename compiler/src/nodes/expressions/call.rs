use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, Node, NodeRef, Render},
    nodes::{VariableExpressionNode, parse_atomic_expression, visit_expression},
    syntax::{ParseError, Parser},
    typecheck::TypeConstraint,
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct IsUnitCall;

impl Fact for IsUnitCall {}

impl Render for IsUnitCall {}

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

                visitor.insert(node, IsUnitCall);

                return;
            }
        }

        for input in &self.inputs {
            visitor.visit(input);
            visitor.edge(input, node, "input");
        }

        visitor.visit(&self.function);
        visitor.edge(&self.function, node, "function");

        visitor.constraint(TypeConstraint::new(
            self.function.clone(),
            visitor
                .db
                .function_type(self.inputs.iter().cloned(), node.clone()),
        ));
    }
}

impl Codegen for CallExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let is_unit = ctx.db.contains::<IsUnitCall>(ctx.current_node());

        if is_unit {
            ctx.write_string("await (");
            ctx.write(&self.inputs[0])?;
            ctx.write_string(")(");
            ctx.write(&self.function)?;
            ctx.write_string(")");
        } else {
            ctx.write_string("await (");
            ctx.write(&self.function)?;
            ctx.write_string(")(");

            for input in &self.inputs {
                ctx.write(input)?;
                ctx.write_string(", ");
            }

            ctx.write_string(")");
        }

        Ok(())
    }
}
