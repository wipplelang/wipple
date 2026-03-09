use crate::{
    codegen::{Codegen, CodegenCtx, ir},
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
                if visitor.capture(&definition.node) {
                    visitor.insert(&definition.node, IsCaptured);
                }

                visitor.graph.replace(node, &definition.node);
                visitor.constraint(GroupConstraint::new(node.clone(), definition.node));
                visitor.insert(node, ResolvedVariable::Variable(resolved_node));
            }
            Definition::Constant(definition) => {
                visitor.graph.instantiate(node, &definition.node, node);

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
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let resolution = ctx.get::<ResolvedVariable>(node)?;

        match resolution {
            ResolvedVariable::Variable(resolved) => {
                let is_mutated = ctx.get::<IsMutated>(&resolved).is_some();

                if is_mutated {
                    ir::Expression::Mutable(resolved).at(node, ctx)
                } else {
                    ir::Expression::Variable(resolved).at(node, ctx)
                }
            }
            ResolvedVariable::Constant(definition) => {
                let bounds = ctx.get::<Bounds>(node).unwrap_or_default();
                codegen_constant(ctx, node, &definition, bounds)
            }
        }
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}

pub fn codegen_constant(
    ctx: &mut CodegenCtx<'_>,
    source: &NodeRef,
    definition: &NodeRef,
    bounds: Bounds,
) -> Option<ir::SpannedExpression> {
    ctx.mark_reachable(source, definition);

    let bounds = bounds
        .0
        .iter()
        .map(|(bound_node, item)| {
            let instance = item.instance.as_ref()?;
            Some((bound_node.clone(), codegen_instance(ctx, source, instance)?))
        })
        .collect::<Option<Vec<_>>>()?;

    ir::Expression::Constant(definition.clone(), bounds).at(source, ctx)
}

pub fn codegen_instance(
    ctx: &mut CodegenCtx<'_>,
    source: &NodeRef,
    instance: &BoundsItemInstance,
) -> Option<ir::SpannedExpression> {
    if instance.from_bound {
        ir::Expression::Bound(instance.instance_node.clone()).at(source, ctx)
    } else {
        let bounds = ctx
            .get::<Bounds>(&instance.resolved_node)
            .unwrap_or_default();

        codegen_constant(ctx, source, &instance.instance_node, bounds)
    }
}
