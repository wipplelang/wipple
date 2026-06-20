use crate::expressions::{variable_expression::DefinitionConstraintTrace, visit_expression};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    render::{Render, RenderCtx},
    span::{Span, Str},
    typecheck::{
        bounds::{Bound, Bounds, ResolvedBound, UnresolvedBound},
        constraints::{
            ConstraintTrace, bound_constraint::BoundConstraint,
            instantiate_constraint::InstantiateConstraint, ty_constraint::TyConstraint,
        },
        groups::Typed,
        ty::{ConstructedTy, Ty},
    },
    visit::{
        Visit, Visitor,
        definitions::{MarkerConstructorDefinition, TraitDefinition, VariantConstructorDefinition},
    },
};
use wipple_parse::{
    names::parse_constructor_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstructorExpression {
    pub span: Span,
    pub constructor: Str,
}

pub fn parse_constructor_expression(
    parser: &mut Parser<'_>,
) -> Result<ConstructorExpression, ParseError> {
    let span = parser.spanned();
    let constructor = parse_constructor_name(parser)?;
    Ok(ConstructorExpression {
        span: span(parser),
        constructor,
    })
}

#[typetag::serde]
impl Visit for ConstructorExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        #[derive(Debug)]
        enum ConstructorDefinition {
            Trait,
            Variant(VariantConstructorDefinition),
            Marker,
        }

        let definition = visitor.resolve_matching(db, &self.constructor, node, |_, definition| {
            if definition.downcast_ref::<TraitDefinition>().is_some() {
                return Some(ConstructorDefinition::Trait);
            }

            if let Some(definition) = definition.downcast_ref::<VariantConstructorDefinition>() {
                return Some(ConstructorDefinition::Variant(definition.clone()));
            }

            if definition
                .downcast_ref::<MarkerConstructorDefinition>()
                .is_some()
            {
                return Some(ConstructorDefinition::Marker);
            }

            None
        });

        let Some((definition_node, definition)) = definition else {
            return;
        };

        db.graph.edge(definition_node, node, "instantiated");

        let substitutions = visitor.substitutions(
            BTreeMap::from([(definition_node, node)]),
            Default::default(),
        );

        visitor.constraint(
            db,
            InstantiateConstraint::new(node, definition_node, substitutions),
        );

        match definition {
            ConstructorDefinition::Trait => {
                visitor.constraint(
                    db,
                    BoundConstraint::new(
                        node,
                        Bound {
                            source_node: node,
                            bound_path: Vec::new(),
                            bound_node: node,
                            trait_node: definition_node,
                            substitutions,
                            is_optional: false,
                        },
                    )
                    .with_trace(DefinitionConstraintTrace {
                        variable: false,
                        definition: definition_node,
                        node,
                    })
                    .with_trace(TraitConstraintTrace {
                        node,
                        trait_node: definition_node,
                    }),
                );

                visitor.codegen(
                    db,
                    node,
                    ConstructorExpressionCodegen::Trait {
                        node,
                        is_generic: visitor.current_definition.is_some(),
                    },
                );
            }
            ConstructorDefinition::Variant(definition) => {
                let elements = definition
                    .elements
                    .iter()
                    .map(|_| {
                        let node = db.node();
                        db.insert(node, Typed::default());
                        node
                    })
                    .collect::<Vec<_>>();

                let result = if definition.elements.is_empty() {
                    node
                } else {
                    let result = db.node();
                    db.insert(result, Typed::default());

                    visitor.constraint(
                        db,
                        TyConstraint::new(
                            node,
                            Ty::Constructed(ConstructedTy::function(elements.clone(), result)),
                        ),
                    );

                    result
                };

                visitor.codegen(
                    db,
                    node,
                    ConstructorExpressionCodegen::Variant {
                        node,
                        name: definition.name,
                        index: definition.index,
                        elements,
                        result,
                    },
                );
            }
            ConstructorDefinition::Marker => {
                visitor.codegen(db, node, ConstructorExpressionCodegen::Marker { node });
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TraitConstraintTrace {
    node: Node,
    trait_node: Node,
}

#[typetag::serde]
impl ConstraintTrace for TraitConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.node]
    }

    fn nodes(&self, db: &Db) -> Vec<Node> {
        [self.node]
            .into_iter()
            .chain(
                find_bound(db, self.node)
                    .into_iter()
                    .flat_map(|bound| bound_parameters(bound).keys().copied()),
            )
            .collect()
    }
}

impl Render for TraitConstraintTrace {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx<'_>) {
        let bound = UnresolvedBound {
            trait_node: self.trait_node,
            parameters: find_bound(db, self.node)
                .map(bound_parameters)
                .cloned()
                .unwrap_or_default(),
        };

        ctx.node(self.node);
        ctx.string(" requires the instance ");
        bound.render_into(db, ctx);
        ctx.string(".");
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ConstructorExpressionCodegen {
    Trait {
        node: Node,
        is_generic: bool,
    },
    Variant {
        node: Node,
        name: Str,
        index: usize,
        elements: Vec<Node>,
        result: Node,
    },
    Marker {
        node: Node,
    },
}

#[typetag::serde]
impl CodegenValue for ConstructorExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match self {
            ConstructorExpressionCodegen::Marker { node } => {
                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Marker,
                });
            }
            ConstructorExpressionCodegen::Trait { node, is_generic } => {
                let bounds = db.get::<Bounds>(*node).cloned().unwrap_or_default();

                match ctx.codegen_instance(db, &[], &bounds, *is_generic)? {
                    ir::Instance::Bound(bound) => {
                        ctx.instruction(ir::Instruction::Value {
                            node: *node,
                            value: ir::Value::Bound(bound),
                        });
                    }
                    ir::Instance::Definition(key) => {
                        ctx.instruction(ir::Instruction::Value {
                            node: *node,
                            value: ir::Value::Constant(ir::DefinitionKey::Constant(key)),
                        });
                    }
                }
            }
            ConstructorExpressionCodegen::Variant {
                node,
                name,
                index,
                elements,
                result,
            } => {
                if elements.is_empty() {
                    ctx.instruction(ir::Instruction::Value {
                        node: *node,
                        value: ir::Value::Variant {
                            name: name.to_string(),
                            index: *index,
                            elements: Vec::new(),
                        },
                    });
                } else {
                    ctx.instruction(ir::Instruction::Value {
                        node: *node,
                        value: ir::Value::Function(ir::Function {
                            inputs: elements.clone(),
                            instructions: vec![
                                ir::Instruction::Value {
                                    node: *result,
                                    value: ir::Value::Variant {
                                        name: name.to_string(),
                                        index: *index,
                                        elements: elements.clone(),
                                    },
                                },
                                ir::Instruction::Return { value: *result },
                            ],
                            closure: Some((*node, Vec::new())),
                        }),
                    });
                }
            }
        }

        Ok(())
    }
}

fn find_bound(db: &Db, node: Node) -> Option<Result<&ResolvedBound, &UnresolvedBound>> {
    db.get(node)
        .and_then(|Bounds(bounds)| bounds.get([node].as_slice()))
        .map(|result| result.as_ref())
}

fn bound_parameters<'a>(
    result: Result<&'a ResolvedBound, &'a UnresolvedBound>,
) -> &'a BTreeMap<Node, Ty> {
    match result {
        Ok(resolved) => &resolved.instance.parameters,
        Err(unresolved) => &unresolved.parameters,
    }
}
