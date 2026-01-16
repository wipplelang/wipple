use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Fact, Node, NodeRef, Render},
    nodes::visit_expression,
    syntax::{ParseError, Parser, parse_variable_name},
    typecheck::{
        GroupConstraint, Instantation, InstantiateConstraint, Replacements, Substitutions,
    },
    visit::{Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub enum ResolvedVariable {
    Variable(NodeRef),
    Constant {
        node: NodeRef,
        substitutions: Substitutions,
    },
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
                visitor.graph.replace(&resolved_node, node);
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
                }));

                visitor.insert(
                    node,
                    ResolvedVariable::Constant {
                        node: definition.node,
                        substitutions,
                    },
                );
            }
            _ => {}
        }
    }
}

impl Codegen for VariableExpressionNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let resolution = ctx
            .db
            .get::<ResolvedVariable>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        match resolution {
            ResolvedVariable::Variable(node) => {
                ctx.write_node(&node);
                Ok(())
            }
            ResolvedVariable::Constant {
                node,
                substitutions,
            } => {
                let mut parameters = substitutions.keys();

                parameters.sort_by_key(|parameter| ctx.db.span(parameter));

                ctx.write_string("await __wipple_constant(");
                ctx.write_node(&node);
                ctx.write_string(", __wipple_types, {");

                for parameter in parameters {
                    let substitution = substitutions.get(&parameter).ok_or_else(|| ctx.error())?;

                    ctx.write_node(&parameter);
                    ctx.write_string(": ");
                    ctx.write_type(&substitution)?;
                    ctx.write_string(", ");
                }

                ctx.write_string("})");

                Ok(())
            }
        }
    }

    fn identifier(&self) -> Option<String> {
        Some(self.variable.clone())
    }
}
