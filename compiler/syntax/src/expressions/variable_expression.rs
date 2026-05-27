use crate::expressions::visit_expression;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    codegen::{CodegenCtx, CodegenError, CodegenValue, ir},
    db::{Db, Node},
    span::{Span, Str},
    typecheck::{
        bounds::Bounds,
        constraints::{
            group_constraint::GroupConstraint, instantiate_constraint::InstantiateConstraint,
        },
        instantiate::InstantiatedParameters,
    },
    visit::{
        IsCaptured, IsMutated, Visit, Visitor,
        definitions::{ConstantDefinition, VariableDefinition},
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

                visitor.record_instantiated_parameters(substitutions);

                visitor.constraint(
                    db,
                    InstantiateConstraint {
                        source_node: node,
                        definition: definition_node,
                        substitutions,
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
                let InstantiatedParameters(parameters) = db.get(*node).cloned().unwrap_or_default();

                let Bounds(bounds) = db.get(*node).cloned().unwrap_or_default();

                let key =
                    ctx.codegen_constant(db, *definition, &parameters, bounds, *is_generic)?;

                ctx.instruction(ir::Instruction::Value {
                    node: *node,
                    value: ir::Value::Constant(ir::DefinitionKey::Constant(key)),
                });
            }
        }

        Ok(())
    }
}
