use crate::{
    attributes::attribute::parse_attributes,
    constraints::{
        bound_constraint::{BoundConstraint, parse_bound_constraint},
        parse_constraints,
    },
    expressions::parse_expression,
    statements::parse_comments,
};

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use wipple_core::{
    arcstr::Substr,
    db::{Db, Fact, Node},
    render::Render,
    span::Span,
    typecheck::{
        bounds::{Instance, Instances},
        constraints::{
            group_constraint::GroupConstraint, instantiate_constraint::InstantiateConstraint,
        },
        ty::Ty,
    },
    util::exact_for_each,
    visit::{
        Visit, Visitor,
        definitions::{
            self, Definition, InstanceDefinitionAttributes, InstanceTrait, TraitDefinition,
        },
    },
};
use wipple_parse::{
    lexer::TokenKind,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MissingInstanceValue;

#[typetag::serde]
impl Fact for MissingInstanceValue {}

impl Render for MissingInstanceValue {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtraInstanceValue;

#[typetag::serde]
impl Fact for ExtraInstanceValue {}

impl Render for ExtraInstanceValue {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceDefinition {
    pub span: Span,
    pub comments: Vec<Substr>,
    pub attributes: Vec<Box<dyn Visit>>,
    pub bound: BoundConstraint,
    pub constraints: Vec<Box<dyn Visit>>,
    pub value: Option<Box<dyn Visit>>,
}

pub fn parse_instance_definition_statement(
    parser: &mut Parser,
) -> Result<InstanceDefinition, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let span = parser.spanned();
    let (bound, constraints) = parse_instance_constraints(parser)?;
    let instance_span = span(parser);
    let value = parser.parse_optional(|parser| {
        parser.token(TokenKind::AssignOperator)?;
        parser.commit("in this instance definition");
        parser.consume_line_breaks();
        parse_expression(parser)
    })?;
    Ok(InstanceDefinition {
        span: instance_span,
        comments,
        attributes,
        bound,
        constraints,
        value,
    })
}

pub fn parse_instance_constraints(
    parser: &mut Parser,
) -> Result<(BoundConstraint, Vec<Box<dyn Visit>>), ParseError> {
    parser.token(TokenKind::InstanceKeyword)?;
    parser.commit("in this instance definition");
    let bound = parse_bound_constraint(parser)?;
    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();
    Ok((bound, constraints))
}

#[typetag::serde]
impl Visit for InstanceDefinition {
    fn span(&self) -> &Span {
        &self.span
    }

    fn visit_definitions(
        &self,
        db: &mut Db,
        node: Node,
        visitor: &mut Visitor,
    ) -> Vec<(Node, Box<dyn Definition>)> {
        let attributes = self
            .attributes
            .iter()
            .map(|attribute| visitor.visit(db, attribute.clone()))
            .collect::<Vec<_>>();

        vec![(
            node,
            Box::new(definitions::InstanceDefinition {
                comments: self.comments.clone(),
                attributes: InstanceDefinitionAttributes::parse(db, &attributes),
                value: self.value.as_ref().map(|_| db.node()),
            }),
        )]
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visitor.within_definition::<definitions::InstanceDefinition>(
            db,
            node,
            |db, visitor, definition| {
                visitor.push_scope(db, node);

                let mut trait_node = None;
                let mut instance_parameters = BTreeMap::new();

                if let Some((definition_node, definition)) =
                    visitor.resolve_as::<TraitDefinition>(db, &self.bound.trait_name, node)
                {
                    trait_node = Some(definition_node);

                    let parameters = self.bound.parameters;
                    let constraints = self.constraints;
                    visitor.with_definition_flag(
                        |d| &mut d.implicit_type_parameters,
                        |visitor| {
                            {
                                let parameters = parameters
                                    .into_iter()
                                    .map(|parameter| visitor.visit(db, parameter))
                                    .collect::<Vec<_>>();

                                let (missing, extra) = exact_for_each(
                                    &definition.parameters,
                                    &parameters,
                                    |&parameter, &substitution| {
                                        instance_parameters
                                            .insert(parameter, Ty::Node(substitution));
                                    },
                                );

                                if !missing.is_empty() {
                                    db.insert(node, crate::types::MissingTypes(missing.to_vec()));
                                }

                                for &parameter in extra {
                                    db.insert(parameter, crate::types::ExtraType);
                                }
                            }

                            for constraint in constraints {
                                let constraint = visitor.visit(db, constraint);
                                db.graph.edge(constraint, node, "constraint");
                            }

                            let substitutions = visitor.substitutions(
                                BTreeMap::from([(definition_node, node)]),
                                instance_parameters.clone(),
                            );

                            visitor.constraint(
                                db,
                                InstantiateConstraint {
                                    source_node: node,
                                    definition: definition_node,
                                    substitutions,
                                },
                            );
                        },
                    );
                }

                if trait_node.is_some() {
                    if let Some(instance_value) = self.value {
                        visitor.with_definition_flag(
                            |d| &mut d.within_constant_value,
                            |visitor| {
                                let instance_value_node = definition.value.unwrap();

                                visitor.visit_as(db, instance_value, instance_value_node);

                                db.graph.edge(instance_value_node, node, "value");

                                visitor.constraint(
                                    db,
                                    GroupConstraint::new(instance_value_node, node),
                                );
                            },
                        );

                        if definition.attributes.error {
                            db.insert(node, ExtraInstanceValue);
                        }
                    } else if !definition.attributes.error {
                        db.insert(node, MissingInstanceValue);
                    }
                }

                if let Some(trait_node) = trait_node {
                    db.insert(node, InstanceTrait(trait_node));

                    db.get_mut_or_default::<Instances>(trait_node)
                        .0
                        .push(Instance {
                            node,
                            trait_node,
                            parameters: instance_parameters,
                            is_from_bound: false,
                        });
                }

                visitor.pop_scope(db);
            },
        );
    }
}
