use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
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
    Trait,
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

        let substitutions = Substitutions::new();

        let definition_node = definition.node();
        visitor.constraint(InstantiateConstraint::new(Instantation {
            source_node: node.clone(),
            definition: definition_node.clone(),
            substitutions: substitutions.clone(),
            replacements: Replacements::from_iter([(definition_node, node.clone())]),
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

                visitor.insert(node, ResolvedConstructor::Trait);
            }
            Definition::VariantConstructor(definition) => {
                visitor.insert(node, ResolvedConstructor::Variant(definition.node));
            }
            Definition::MarkerConstructor(_) => {
                visitor.insert(node, ResolvedConstructor::Marker);
            }
            _ => {}
        }
    }
}

impl Codegen for ConstructorExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let constructor = ctx
            .db
            .get::<ResolvedConstructor>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        match constructor {
            ResolvedConstructor::Marker => {
                ctx.write_string("null");
                Ok(())
            }
            ResolvedConstructor::Trait => {
                let bounds = ctx.db.get::<Bounds>(ctx.current_node()).unwrap_or_default();

                let instance = bounds
                    .0
                    .get(ctx.current_node())
                    .and_then(|bound| bound.instance.as_ref())
                    .ok_or_else(|| ctx.error())?
                    .clone();

                codegen_instance(ctx, &instance)?;

                Ok(())
            }
            ResolvedConstructor::Variant(node) => {
                ctx.write(&node)?;
                Ok(())
            }
        }
    }
}
