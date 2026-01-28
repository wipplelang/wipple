use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, Node, NodeRef, Render},
    nodes::visit_expression,
    syntax::{ParseError, Parser, parse_variable_name},
    typecheck::{
        Bounds, BoundsItemInstance, GroupConstraint, Instantation, InstantiateConstraint,
        Replacements, Substitutions,
    },
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub enum ResolvedVariable {
    Variable(NodeRef),
    Constant(NodeRef),
}

impl Fact for ResolvedVariable {}

impl Render for ResolvedVariable {}

#[derive(Debug)]
pub struct VariableExpressionNode {
    pub variable: String,
}

impl Node for VariableExpressionNode {}

pub fn parse_variable_expression(
    parser: &mut Parser<'_>,
) -> Result<VariableExpressionNode, ParseError> {
    let variable = parse_variable_name(parser)?;

    Ok(VariableExpressionNode { variable })
}

impl Visit for VariableExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let Some(definition) =
            visitor.resolve(&self.variable, node, |definition| match definition {
                Definition::Variable(definition) => Some(Definition::Variable(definition.clone())),
                Definition::Constant(definition) => Some(Definition::Constant(definition.clone())),
                _ => None,
            })
        else {
            return;
        };

        let resolved_node = definition.node();

        match definition {
            Definition::Variable(definition) => {
                visitor.graph.replace(&resolved_node, node);
                visitor.constraint(GroupConstraint::new(node.clone(), definition.node));

                visitor.insert(node, ResolvedVariable::Variable(resolved_node));
            }
            Definition::Constant(definition) => {
                visitor.constraint(InstantiateConstraint::new(Instantation {
                    source_node: node.clone(),
                    definition: definition.node.clone(),
                    substitutions: Substitutions::new(),
                    replacements: Replacements::from_iter([(
                        definition.node.clone(),
                        node.clone(),
                    )]),
                }));

                visitor.insert(node, ResolvedVariable::Constant(definition.node));
            }
            _ => {}
        }
    }
}

impl Codegen for VariableExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let resolution = ctx
            .db
            .get::<ResolvedVariable>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        match resolution {
            ResolvedVariable::Variable(node) => ctx.write_node(&node),
            ResolvedVariable::Constant(node) => {
                let bounds = ctx.db.get::<Bounds>(ctx.current_node()).unwrap_or_default();

                codegen_constant(ctx, &node, bounds, |ctx| ctx.write_node(&node))?;
            }
        }

        Ok(())
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}

pub fn codegen_constant(
    ctx: &mut CodegenCtx<'_>,
    node: &NodeRef,
    bounds: Bounds,
    write_node: impl FnOnce(&mut CodegenCtx<'_>),
) -> Result<(), CodegenError> {
    ctx.mark_reachable(node);

    ctx.write_string("await ");
    write_node(ctx);
    ctx.write_string("({ ");
    for (bound_node, item) in bounds.0 {
        let instance = item.instance.as_ref().ok_or_else(|| ctx.error())?;

        ctx.write_node(&bound_node);
        ctx.write_string(": ");
        codegen_instance(ctx, instance)?;

        ctx.write_string(", ");
    }

    ctx.write_string(" })");

    Ok(())
}

pub fn codegen_instance(
    ctx: &mut CodegenCtx<'_>,
    instance: &BoundsItemInstance,
) -> Result<(), CodegenError> {
    if instance.from_bound {
        ctx.write_string("__wipple_bounds.");
        ctx.write_node(&instance.instance_node);
        Ok(())
    } else {
        let bounds = ctx
            .db
            .get::<Bounds>(&instance.resolved_node)
            .unwrap_or_default();

        codegen_constant(ctx, &instance.instance_node, bounds, |ctx| {
            ctx.write_node(&instance.instance_node)
        })
    }
}
