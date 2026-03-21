use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{codegen_instance, visit_expression},
    syntax::{ParseError, Parser, parse_constructor_name},
    typecheck::{
        Bound, BoundConstraint, Bounds, Instantation, InstantiateConstraint, Replacements,
        Substitutions, TypeConstraint,
    },
    visit::{Definition, VariantConstructorDefinition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub enum ResolvedConstructor {
    Marker,
    Trait(bool),
    Variant(VariantConstructorDefinition, NodeRef),
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
                        target_node: None,
                        substitutions,
                        optional: false,
                    },
                ));

                let generic = visitor.try_current_definition().is_some();

                visitor.insert(node, ResolvedConstructor::Trait(generic));
            }
            Definition::VariantConstructor(definition) => {
                let result = if definition.elements.is_empty() {
                    node.clone()
                } else {
                    let span = visitor.span(node);

                    let element_temporaries = definition
                        .elements
                        .iter()
                        .map(|node| {
                            let span = visitor.span(node);
                            visitor.node(span, HiddenNode::default())
                        })
                        .collect::<Vec<_>>();

                    let result_temporary = visitor.node(span, HiddenNode::default());

                    visitor.constraint(TypeConstraint::new(
                        node.clone(),
                        visitor.function_type(element_temporaries, result_temporary.clone()),
                    ));

                    result_temporary
                };

                visitor.insert(node, ResolvedConstructor::Variant(definition, result));
            }
            Definition::MarkerConstructor(_) => {
                visitor.insert(node, ResolvedConstructor::Marker);
            }
            _ => {}
        }
    }
}

impl Codegen for ConstructorExpressionNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let constructor = ctx
            .get::<ResolvedConstructor>(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        match constructor {
            ResolvedConstructor::Marker => {
                ctx.instruction(ir::Instruction::Value {
                    node: node.clone(),
                    value: ir::Value::Marker,
                });
            }
            ResolvedConstructor::Trait(generic) => {
                let bounds = ctx.get::<Bounds>(node).unwrap_or_default();

                let instance = bounds
                    .0
                    .get(node)
                    .and_then(|bound| bound.instance.as_ref())
                    .ok_or_else(|| anyhow::format_err!("unresolved"))?;

                let instance = codegen_instance(ctx, instance, generic)?;

                match instance {
                    ir::Instance::Bound(bound) => {
                        ctx.instruction(ir::Instruction::Value {
                            node: node.clone(),
                            value: ir::Value::Bound(bound),
                        });
                    }
                    ir::Instance::Definition(key) => {
                        ctx.instruction(ir::Instruction::Value {
                            node: node.clone(),
                            value: ir::Value::Constant(key),
                        });
                    }
                }
            }
            ResolvedConstructor::Variant(definition, result) => {
                if definition.elements.is_empty() {
                    ctx.instruction(ir::Instruction::Value {
                        node: node.clone(),
                        value: ir::Value::Variant {
                            index: definition.index,
                            elements: Vec::new(),
                        },
                    });
                } else {
                    ctx.instruction(ir::Instruction::Value {
                        node: node.clone(),
                        value: ir::Value::Function {
                            inputs: definition.elements.clone(),
                            captures: Vec::new(),
                            instructions: vec![
                                ir::Instruction::Value {
                                    node: result.clone(),
                                    value: ir::Value::Variant {
                                        index: definition.index,
                                        elements: definition.elements,
                                    },
                                },
                                ir::Instruction::Return { value: result },
                            ],
                        },
                    });
                }
            }
        }

        Ok(())
    }
}
