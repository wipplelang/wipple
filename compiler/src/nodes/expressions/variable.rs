use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::visit_expression,
    syntax::{ParseError, Parser, parse_variable_name},
    typecheck::{
        Bounds, BoundsItemInstance, GroupConstraint, Instantation, InstantiateConstraint,
        Replacements, Substitutions,
    },
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct IsMutated;

impl Fact for IsMutated {}

impl Render for IsMutated {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is mutated")
    }
}

#[derive(Debug, Clone)]
pub struct IsCaptured;

impl Fact for IsCaptured {}

impl Render for IsCaptured {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is captured")
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedVariable {
    Variable(NodeRef),
    Constant(NodeRef, Substitutions, bool),
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
                if visitor.capture(&definition.node) {
                    visitor.insert(&definition.node, IsCaptured);
                }

                visitor.graph.replace(node, &definition.node);
                visitor.constraint(GroupConstraint::new(node.clone(), definition.node));
                visitor.insert(node, ResolvedVariable::Variable(resolved_node));
            }
            Definition::Constant(definition) => {
                let substitutions = Substitutions::new();

                visitor.constraint(InstantiateConstraint::new(Instantation {
                    source_node: node.clone(),
                    definition: definition.node.clone(),
                    substitutions: substitutions.clone(),
                    replacements: Replacements::from_iter([(
                        definition.node.clone(),
                        node.clone(),
                    )]),
                    from_expression: true,
                }));

                let generic = visitor.try_current_definition().is_some();

                visitor.insert(
                    node,
                    ResolvedVariable::Constant(definition.node, substitutions, generic),
                );
            }
            _ => {}
        }
    }
}

impl Codegen for VariableExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let resolved = ctx
            .get::<ResolvedVariable>(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        match resolved {
            ResolvedVariable::Variable(resolved) => {
                let is_mutated = ctx.get::<IsMutated>(&resolved).is_some();

                if is_mutated {
                    ctx.instruction(ir::Instruction::Value {
                        node: node.clone(),
                        value: ir::Value::MutableVariable(resolved),
                    });
                } else {
                    ctx.instruction(ir::Instruction::Value {
                        node: node.clone(),
                        value: ir::Value::Variable(resolved),
                    });
                }
            }
            ResolvedVariable::Constant(definition, substitutions, generic) => {
                let bounds = ctx.get::<Bounds>(node).unwrap_or_default();

                let key = codegen_constant(ctx, &definition, &substitutions, bounds, generic)?;

                ctx.instruction(ir::Instruction::Value {
                    node: node.clone(),
                    value: ir::Value::Constant(key),
                });
            }
        }

        Ok(())
    }
}

pub fn codegen_constant(
    ctx: &mut CodegenCtx<'_>,
    definition: &NodeRef,
    substitutions: &Substitutions,
    bounds: Bounds,
    generic: bool,
) -> CodegenResult<ir::DefinitionKey> {
    let bounds = bounds
        .0
        .iter()
        .map(|(bound_node, item)| {
            let instance = codegen_instance(
                ctx,
                item.instance
                    .as_ref()
                    .ok_or_else(|| anyhow::format_err!("unresolved"))?,
                true, // don't record keys for bounds
            )?;

            Ok((bound_node.clone(), instance))
        })
        .collect::<CodegenResult<_>>()?;

    ctx.definition_key(definition, substitutions, bounds, generic)
}

pub fn codegen_instance(
    ctx: &mut CodegenCtx<'_>,
    instance: &BoundsItemInstance,
    generic: bool,
) -> CodegenResult<ir::Instance> {
    if instance.from_bound {
        Ok(ir::Instance::Bound(instance.instance_node.clone()))
    } else {
        let bounds = ctx
            .get::<Bounds>(&instance.resolved_node)
            .unwrap_or_default();

        Ok(ir::Instance::Definition(codegen_constant(
            ctx,
            &instance.instance_node,
            &instance.instance_substitutions,
            bounds,
            generic,
        )?))
    }
}
