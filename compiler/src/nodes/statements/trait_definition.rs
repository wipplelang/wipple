use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Node, NodeRef},
    nodes::{
        parse_atomic_type, parse_attributes, parse_comments, parse_constraints,
        parse_type_parameters,
    },
    syntax::{ParseError, Parser, TokenKind, parse_type_name},
    typecheck::GroupConstraint,
    visit::{TraitAttributes, TraitDefinition, Visit, Visitor},
};

#[derive(Debug)]
pub struct TraitDefinitionNode {
    pub comments: Vec<String>,
    pub attributes: Vec<NodeRef>,
    pub name: String,
    pub parameters: Vec<NodeRef>,
    pub ty: NodeRef,
    pub constraints: Vec<NodeRef>,
}

impl Node for TraitDefinitionNode {}

pub fn parse_trait_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<TraitDefinitionNode, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let name = parse_type_name(parser)?;

    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();

    let parameters = parse_type_parameters(parser)?;
    let (ty, constraints) = parse_trait_constraints(parser)?;

    Ok(TraitDefinitionNode {
        comments,
        attributes,
        name,
        parameters,
        ty,
        constraints,
    })
}

pub fn parse_trait_constraints(
    parser: &mut Parser<'_>,
) -> Result<(NodeRef, Vec<NodeRef>), ParseError> {
    parser.token(TokenKind::TraitKeyword)?;
    parser.commit("in this trait definition");

    let ty = parse_atomic_type(parser)?;
    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();

    Ok((ty, constraints))
}

impl Visit for TraitDefinitionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visitor.defining(node, |visitor| {
            visitor.push_scope();

            visitor.with_implicit_type_parameters(|visitor| {
                for parameter in &self.parameters {
                    visitor.visit(parameter);
                    visitor.edge(parameter, node, "parameter");
                }
            });

            let ty = self.ty.clone();
            let constraints = self.constraints.clone();
            let node_for_closure = node.clone();

            visitor.after_all_definitions(move |visitor| {
                let node = node_for_closure.clone();
                visitor.visit(&ty);
                visitor.edge(&ty, &node, "type");

                visitor.constraint(GroupConstraint::new(node.clone(), ty.clone()));

                for constraint in &constraints {
                    visitor.visit(constraint);
                    visitor.edge(constraint, &node, "constraint");
                }
            });

            visitor.pop_scope();

            let definition = TraitDefinition {
                name: self.name.clone(),
                node: node.clone(),
                comments: self.comments.clone(),
                attributes: TraitAttributes {},
                parameters: self.parameters.clone(),
            };

            visitor.define(&definition.name, definition.clone());

            definition
        });
    }
}

impl Codegen for TraitDefinitionNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Ok(())
    }
}
