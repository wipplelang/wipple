use crate::expressions::visit_expression;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    render::{Comments, Render, RenderCtx},
    span::{Span, Str},
    typecheck::{
        bounds::Bounds,
        constraints::{
            ConstraintTrace, group_constraint::GroupConstraint,
            instantiate_constraint::InstantiateConstraint,
        },
    },
    util::{get_links, instantiated_node_for},
    visit::{
        IsCaptured, IsMutated, TypeParameters, Visit, Visitor,
        definitions::{ConstantDefinition, Defined, VariableDefinition},
    },
};
use wipple_parse::{
    names::parse_variable_name,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableExpression {
    pub span: Span,
    pub variable: Str,
}

pub fn parse_variable_expression(
    parser: &mut Parser<'_>,
) -> Result<VariableExpression, ParseError> {
    let span = parser.spanned();
    let variable = parse_variable_name(parser)?;
    Ok(VariableExpression {
        span: span(parser),
        variable,
    })
}

#[typetag::serde]
impl Visit for VariableExpression {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_expression(db, node, visitor);

        #[derive(Debug)]
        enum Definition {
            Variable,
            Constant,
        }

        let definition = visitor.resolve_matching(db, &self.variable, node, |_, definition| {
            if definition.downcast_ref::<VariableDefinition>().is_some() {
                return Some(Definition::Variable);
            }

            if definition.downcast_ref::<ConstantDefinition>().is_some() {
                return Some(Definition::Constant);
            }

            None
        });

        let Some((definition_node, definition)) = definition else {
            return;
        };

        match definition {
            Definition::Variable => {
                if visitor.capture(definition_node) {
                    db.insert(definition_node, IsCaptured);
                }

                db.graph.replace(node, definition_node);

                visitor.constraint(db, GroupConstraint::new(node, definition_node));
                visitor.codegen(
                    db,
                    node,
                    VariableExpressionCodegen::Variable {
                        node,
                        resolved: definition_node,
                    },
                );
            }
            Definition::Constant => {
                let substitutions = visitor.substitutions(
                    BTreeMap::from([(definition_node, node)]),
                    Default::default(),
                );

                visitor.constraint(
                    db,
                    InstantiateConstraint {
                        source_node: node,
                        definition: definition_node,
                        substitutions,
                        trace: Some(Box::new(ConstantConstraintTrace {
                            definition: definition_node,
                            node,
                        })),
                    },
                );

                visitor.codegen(
                    db,
                    node,
                    VariableExpressionCodegen::Constant {
                        node,
                        definition: definition_node,
                        is_generic: visitor.current_definition.is_some(),
                    },
                );
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct ConstantConstraintTrace {
    definition: Node,
    node: Node,
}

#[typetag::serde]
impl ConstraintTrace for ConstantConstraintTrace {
    fn nodes_mut(&mut self) -> Vec<&mut Node> {
        vec![&mut self.node]
    }

    fn nodes(&self, db: &Db) -> Vec<Node> {
        let TypeParameters(parameters) = db.get(self.definition).cloned().unwrap_or_default();

        [self.node, self.definition]
            .into_iter()
            .chain(
                parameters
                    .into_iter()
                    .filter_map(|parameter| instantiated_node_for(db, parameter, self.node)),
            )
            .collect()
    }

    fn source_node_mut(&mut self) -> Option<&mut Node> {
        Some(&mut self.node)
    }
}

impl Render for ConstantConstraintTrace {
    fn render_into(&self, db: &Db, ctx: &mut RenderCtx) {
        let Some(Defined(definition)) = db.get(self.definition) else {
            return;
        };

        let Some(definition) = definition.downcast_ref::<ConstantDefinition>() else {
            return;
        };

        ctx.comments(
            db,
            self.definition,
            &Comments {
                nodes: Default::default(),
                comments: definition.comments.clone(),
                links: get_links(db, self.definition, self.node),
            },
        );
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum VariableExpressionCodegen {
    Variable {
        node: Node,
        resolved: Node,
    },
    Constant {
        node: Node,
        definition: Node,
        is_generic: bool,
    },
}

#[typetag::serde]
impl CodegenValue for VariableExpressionCodegen {
    fn codegen(&self, db: &Db, ctx: &mut CodegenCtx) -> Result<(), CodegenError> {
        match self {
            VariableExpressionCodegen::Variable { node, resolved } => {
                let value = if db.contains::<IsMutated>(*resolved) {
                    ir::Value::MutableVariable(*resolved)
                } else {
                    ir::Value::Variable(*resolved)
                };

                ctx.instruction(ir::Instruction::Value { node: *node, value });
            }
            VariableExpressionCodegen::Constant {
                node,
                definition,
                is_generic,
            } => {
                let Bounds(bounds) = db.get(*node).cloned().unwrap_or_default();

                let key = ctx.codegen_constant(db, *definition, bounds, *is_generic)?;

                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Constant(ir::DefinitionKey::Constant(key)),
                });
            }
        }

        Ok(())
    }
}
