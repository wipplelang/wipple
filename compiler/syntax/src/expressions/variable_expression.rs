use crate::expressions::visit_expression;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    codegen::{CodegenError, hir},
    db::{Db, Node},
    facts::Description,
    render::Comments,
    span::{Span, Str},
    typecheck::{
        bounds::ResolvedBounds,
        constraints::{instantiate_constraint::InstantiateConstraint, ty_constraint::TyConstraint},
        groups::NodeRank,
        ty::Ty,
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

                visitor.constraint(db, TyConstraint::new(node, Ty::Node(definition_node)));

                // Prefer showing conflicts on the definition rather than its uses
                visitor.rank(node, NodeRank::Annotated);

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
                    InstantiateConstraint::new(node, definition_node, substitutions),
                );

                visitor.codegen(
                    db,
                    node,
                    VariableExpressionCodegen::Constant {
                        node,
                        definition: definition_node,
                    },
                );

                db.insert(
                    node,
                    Description(Comments::for_static(
                        definition_node,
                        "[`definition`] is defined as a [`definition@type`].",
                        [("definition", Some(definition_node))],
                    )),
                );
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum VariableExpressionCodegen {
    Variable { node: Node, resolved: Node },
    Constant { node: Node, definition: Node },
}

#[typetag::serde]
impl hir::Write for VariableExpressionCodegen {
    fn write(&self, db: &Db, ctx: &mut hir::Ctx) -> Result<(), CodegenError> {
        match self {
            VariableExpressionCodegen::Variable { node, resolved } => {
                let value = if db.contains::<IsMutated>(*resolved) {
                    hir::Value::MutableVariable(*resolved)
                } else {
                    hir::Value::Variable(*resolved)
                };

                ctx.instruction(hir::Instruction::Value { node: *node, value });
            }
            VariableExpressionCodegen::Constant { node, definition } => {
                let bounds = db.get::<ResolvedBounds>(*node).cloned().unwrap_or_default();

                let bounds = ctx.bounds_for_constant(*definition, &[], &bounds)?;

                ctx.instruction(hir::Instruction::Value {
                    node: *node,
                    value: hir::Value::Constant {
                        definition: hir::DefinitionKey::Constant(*definition),
                        bounds,
                    },
                });
            }
        }

        Ok(())
    }
}
