use crate::{
    codegen::{Codegen, CodegenCtx, ir},
    database::{Fact, Node, NodeRef, Render},
    nodes::{codegen_instance, visit_expression},
    syntax::{ParseError, Parser, parse_constructor_name},
    typecheck::{
        Bound, BoundConstraint, Bounds, Instantation, InstantiateConstraint, Replacements,
        Substitutions,
    },
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub enum ResolvedConstructor {
    Marker,
    Trait(bool),
    Variant(NodeRef),
}

impl Fact for ResolvedConstructor {}

impl Render for ResolvedConstructor {}

#[derive(Debug)]
pub struct ConstructorExpressionNode {
    pub constructor: String,
}

impl Node for ConstructorExpressionNode {}

pub fn parse_constructor_expression(
    parser: &mut Parser<'_>,
) -> Result<ConstructorExpressionNode, ParseError> {
    let constructor = parse_constructor_name(parser)?;

    Ok(ConstructorExpressionNode { constructor })
}

impl Visit for ConstructorExpressionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_expression(node, visitor);

        let Some(definition) =
            visitor.resolve(&self.constructor, node, |definition| match definition {
                Definition::Trait(definition) => Some(Definition::Trait(definition.clone())),
                Definition::VariantConstructor(definition) => {
                    Some(Definition::VariantConstructor(definition.clone()))
                }
                Definition::MarkerConstructor(definition) => {
                    Some(Definition::MarkerConstructor(definition.clone()))
                }
                _ => None,
            })
        else {
            return;
        };

        visitor.graph.instantiate(node, &definition.node(), node);

        let substitutions = Substitutions::new();

        let definition_node = definition.node();
        visitor.constraint(InstantiateConstraint::new(Instantation {
            source_node: node.clone(),
            definition: definition_node.clone(),
            substitutions: substitutions.clone(),
            replacements: Replacements::from_iter([(definition_node, node.clone())]),
            from_expression: true,
        }));

        match definition {
            Definition::Trait(trait_definition) => {
                visitor.constraint(BoundConstraint::new(
                    node.clone(),
                    Bound {
                        source_node: node.clone(),
                        bound_node: node.clone(),
                        trait_node: trait_definition.node,
                        substitutions,
                        optional: false,
                    },
                ));

                let generic = visitor.try_current_definition().is_some();

                visitor.insert(node, ResolvedConstructor::Trait(generic));
            }
            Definition::VariantConstructor(definition) => {
                visitor.insert(node, ResolvedConstructor::Variant(definition.variant));
            }
            Definition::MarkerConstructor(_) => {
                visitor.insert(node, ResolvedConstructor::Marker);
            }
            _ => {}
        }
    }
}

impl Codegen for ConstructorExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> Option<ir::SpannedExpression> {
        let constructor = ctx.get::<ResolvedConstructor>(node)?;

        match constructor {
            ResolvedConstructor::Marker => Some(ir::Expression::Marker.at(node, ctx)),
            ResolvedConstructor::Trait(generic) => {
                let bounds = ctx.get::<Bounds>(node).unwrap_or_default();

                let instance = bounds
                    .0
                    .get(node)
                    .and_then(|bound| bound.instance.as_ref())?;

                let instance = codegen_instance(ctx, instance, generic)?;

                match instance {
                    ir::Instance::Bound(bound) => Some(ir::Expression::Bound(bound).at(node, ctx)),
                    ir::Instance::Definition(key) => {
                        Some(ir::Expression::Constant(key).at(node, ctx))
                    }
                }
            }
            ResolvedConstructor::Variant(variant) => {
                // Codegen the variant constructor in the context of the current
                // node so the instantiated type is used
                variant.codegen(node, ctx)
            }
        }
    }
}
