use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{
        BoundConstraintNode, ExtraType, MissingTypes, parse_attributes, parse_bound_constraint,
        parse_comments, parse_constraints, parse_expression,
    },
    syntax::{ParseError, Parser, TokenKind},
    typecheck::{
        GroupConstraint, Instance, Instances, Instantation, InstantiateConstraint, Replacements,
        Substitutions,
    },
    visit::{Defined, Definition, InstanceAttributes, InstanceDefinition, Visit, Visitor},
};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub struct MissingInstanceValue;

impl Fact for MissingInstanceValue {}

impl Render for MissingInstanceValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing instance value")
    }
}

#[derive(Debug, Clone)]
pub struct ExtraInstanceValue;

impl Fact for ExtraInstanceValue {}

impl Render for ExtraInstanceValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is extra instance value")
    }
}

#[derive(Debug)]
pub struct InstanceDefinitionNode {
    pub comments: Vec<String>,
    pub attributes: Vec<NodeRef>,
    pub bound: NodeRef,
    pub constraints: Vec<NodeRef>,
    pub value: Option<NodeRef>,
}

impl Node for InstanceDefinitionNode {}

pub fn parse_instance_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<InstanceDefinitionNode, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let (bound, constraints) = parse_instance_constraints(parser)?;

    let value = parser.parse_optional(|parser| {
        parser.token(TokenKind::AssignOperator)?;
        parser.commit("in this instance definition");
        parser.consume_line_breaks();
        parse_expression(parser)
    })?;

    Ok(InstanceDefinitionNode {
        comments,
        attributes,
        bound,
        constraints,
        value,
    })
}

pub fn parse_instance_constraints(
    parser: &mut Parser<'_>,
) -> Result<(NodeRef, Vec<NodeRef>), ParseError> {
    parser.token(TokenKind::InstanceKeyword)?;
    parser.commit("in this instance definition");

    let bound = parser.node(parse_bound_constraint)?;
    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();

    Ok((bound, constraints))
}

impl Visit for InstanceDefinitionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visitor.defining(node, |visitor| {
            visitor.push_scope();

            let attributes = InstanceAttributes::from_attributes(visitor.db, &self.attributes);

            let trait_node = Rc::new(RefCell::new(None));
            let substitutions = Substitutions::new();
            visitor.after_type_definitions({
                let bound = self.bound.clone();
                let constraints = self.constraints.clone();
                let node = node.clone();
                let trait_node = trait_node.clone();
                let substitutions = substitutions.clone();
                move |visitor| {
                    let Some(bound) = bound.downcast_ref::<BoundConstraintNode>() else {
                        return;
                    };

                    let Some(trait_definition) =
                        visitor.resolve(&bound.trait_name, &node, |definition| match definition {
                            Definition::Trait(definition) => Some(definition.clone()),
                            _ => None,
                        })
                    else {
                        return;
                    };

                    *trait_node.borrow_mut() = Some(trait_definition.node.clone());

                    visitor.with_implicit_type_parameters(|visitor| {
                        for parameter in &bound.parameters {
                            visitor.visit(parameter);
                            visitor.edge(parameter, &node, "parameter");
                        }

                        let mut definition_parameters = trait_definition.parameters.iter().cloned();
                        let mut parameters = bound.parameters.iter().cloned();

                        substitutions.extend(
                            definition_parameters
                                .by_ref()
                                .zip(parameters.by_ref())
                                .map(|(parameter, substitution)| (parameter, substitution.into())),
                        );

                        let missing = definition_parameters.collect::<Vec<_>>();
                        let extra = parameters.collect::<Vec<_>>();

                        if !missing.is_empty() {
                            visitor.insert(&node, MissingTypes(missing));
                        }

                        for node in extra {
                            visitor.insert(&node, ExtraType);
                        }

                        for constraint in &constraints {
                            visitor.visit(constraint);
                            visitor.edge(constraint, &node, "constraint");
                        }

                        visitor.constraint(InstantiateConstraint::new(Instantation {
                            source_node: node.clone(),
                            definition: trait_definition.node.clone(),
                            substitutions: substitutions.clone(),
                            replacements: Replacements::from_iter([(
                                trait_definition.node.clone(),
                                node.clone(),
                            )]),
                        }));
                    });
                }
            });

            let definition = InstanceDefinition {
                node: node.clone(),
                comments: self.comments.clone(),
                attributes: attributes.clone(),
                value: self.value.clone(),
                trait_node: node.clone(),
            };

            visitor.after_all_definitions({
                let node = node.clone();
                let definition = definition.clone();
                let value = self.value.clone();
                move |visitor| {
                    let Some(trait_node) = trait_node.borrow().clone() else {
                        return;
                    };

                    if let Some(value) = value {
                        visitor.within_constant_value(|visitor| {
                            visitor.visit(&value);
                            visitor.edge(&value, &node, "value");
                            visitor.constraint(GroupConstraint::new(value, node.clone()));
                        });

                        if attributes.error {
                            visitor.insert(&node, ExtraInstanceValue);
                        }
                    } else if !attributes.error {
                        visitor.insert(&node, MissingInstanceValue);
                    }

                    visitor.insert(
                        &node,
                        Defined(
                            InstanceDefinition {
                                trait_node: trait_node.clone(),
                                ..definition.clone()
                            }
                            .into(),
                        ),
                    );

                    visitor.with_fact(&trait_node.clone(), |Instances(instances)| {
                        instances.push(Instance {
                            node,
                            trait_node,
                            substitutions,
                            from_bound: false,
                            default: attributes.default,
                            error: attributes.error,
                        });
                    });
                }
            });

            visitor.pop_scope();

            definition
        });
    }
}

impl Codegen for InstanceDefinitionNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Ok(())
    }
}
