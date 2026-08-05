use crate::expressions::{variable_expression::DefinitionConstraintTrace, visit_expression};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{
        bounds::{Bound, ResolvedBounds},
        constraints::{
            bound_constraint::BoundConstraint, instantiate_constraint::InstantiateConstraint,
            ty_constraint::TyConstraint,
        },
        groups::Typed,
        ty::{ConstructedTy, Ty},
    },
    visit::{
        Visit, Visitor,
        definitions::{
            MarkerConstructorDefinition, TraitDefinition, VariantConstructorDefinition,
            WrapperConstructorDefinition,
        },
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

        #[expect(unused)]
        #[derive(Debug)]
        enum ConstructorDefinition {
            Trait(TraitDefinition),
            Marker(MarkerConstructorDefinition),
            Wrapper(WrapperConstructorDefinition),
            Variant(VariantConstructorDefinition),
        }

        let definition = visitor.resolve_matching(db, &self.constructor, node, |_, definition| {
            if let Some(definition) = definition.downcast_ref::<TraitDefinition>() {
                return Some(ConstructorDefinition::Trait(definition.clone()));
            }

            if let Some(definition) = definition.downcast_ref::<MarkerConstructorDefinition>() {
                return Some(ConstructorDefinition::Marker(definition.clone()));
            }

            if let Some(definition) = definition.downcast_ref::<WrapperConstructorDefinition>() {
                return Some(ConstructorDefinition::Wrapper(definition.clone()));
            }

            if let Some(definition) = definition.downcast_ref::<VariantConstructorDefinition>() {
                return Some(ConstructorDefinition::Variant(definition.clone()));
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
            ConstructorDefinition::Trait(_) => {
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
                    }),
                );

                visitor.codegen(db, node, ConstructorExpressionCodegen::Trait { node });
            }
            ConstructorDefinition::Marker(_) => {
                visitor.codegen(db, node, ConstructorExpressionCodegen::Marker { node });
            }
            ConstructorDefinition::Wrapper(_) => {
                let value = db.node();
                db.insert(value, Typed::default());

                let result = db.node();
                db.insert(result, Typed::default());

                visitor.constraint(
                    db,
                    TyConstraint::new(
                        node,
                        Ty::Constructed(ConstructedTy::function(vec![value], result)),
                    ),
                );

                visitor.codegen(
                    db,
                    node,
                    ConstructorExpressionCodegen::Wrapper {
                        node,
                        value,
                        result,
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
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ConstructorExpressionCodegen {
    Trait {
        node: Node,
    },
    Marker {
        node: Node,
    },
    Wrapper {
        node: Node,
        value: Node,
        result: Node,
    },
    Variant {
        node: Node,
        name: Str,
        index: usize,
        elements: Vec<Node>,
        result: Node,
    },
}

#[typetag::serde]
impl CodegenValue for ConstructorExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match self {
            ConstructorExpressionCodegen::Trait { node } => {
                let bounds = db.get::<ResolvedBounds>(*node).cloned().unwrap_or_default();

                match ctx.bound_for_instance(&[], &bounds)? {
                    ir::Instance::Bound(bound) => {
                        ctx.instruction(ir::Instruction::Value {
                            node: *node,
                            value: ir::Value::Bound(bound),
                        });
                    }
                    ir::Instance::Instance { definition, bounds } => {
                        ctx.mark_reachable(definition);

                        ctx.instruction(ir::Instruction::Value {
                            node: *node,
                            value: ir::Value::Constant { definition, bounds },
                        });
                    }
                }
            }
            ConstructorExpressionCodegen::Marker { node } => {
                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Marker,
                });
            }
            ConstructorExpressionCodegen::Wrapper {
                node,
                value,
                result,
            } => {
                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Function(ir::Function {
                        bounds: None,
                        inputs: vec![*value],
                        instructions: vec![
                            ir::Instruction::Value {
                                node: *result,
                                // Wrappers are transparent at runtime
                                value: ir::Value::Variable(*value),
                            },
                            ir::Instruction::Return { value: *result },
                        ],
                        captures: Vec::new(),
                    }),
                });
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
                            bounds: None,
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
                            captures: Vec::new(),
                        }),
                    });
                }
            }
        }

        Ok(())
    }
}
