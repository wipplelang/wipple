use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, Node, NodeRef, Render},
    nodes::{parse_attributes, parse_comments, parse_constraints, parse_type},
    syntax::{ParseError, Parser, TokenKind, parse_variable_name},
    typecheck::GroupConstraint,
    visit::{ConstantAttributes, ConstantDefinition, Defined, Definition, Visit, Visitor},
};

#[derive(Debug, Clone)]
pub struct MissingConstantValue;

impl Fact for MissingConstantValue {}

impl Render for MissingConstantValue {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is missing constant value")
    }
}

#[derive(Debug)]
pub struct ConstantDefinitionNode {
    pub comments: Vec<String>,
    pub attributes: Vec<NodeRef>,
    pub name: String,
    pub ty: NodeRef,
    pub constraints: Vec<NodeRef>,
}

impl Node for ConstantDefinitionNode {}

pub fn parse_constant_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<ConstantDefinitionNode, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let name = parse_variable_name(parser)?;
    let (ty, constraints) = parse_constant_constraints(parser)?;

    Ok(ConstantDefinitionNode {
        comments,
        attributes,
        name,
        ty,
        constraints,
    })
}

pub fn parse_constant_constraints(
    parser: &mut Parser<'_>,
) -> Result<(NodeRef, Vec<NodeRef>), ParseError> {
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this constant definition");
    parser.consume_line_breaks();

    let ty = parse_type(parser)?;

    let constraints = parser
        .parse_optional(parse_constraints)?
        .unwrap_or_default();

    Ok((ty, constraints))
}

impl Visit for ConstantDefinitionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visitor.defining(node, |visitor| {
            visitor.push_scope();

            visitor.after_type_definitions({
                let ty = self.ty.clone();
                let constraints = self.constraints.clone();
                let node = node.clone();
                move |visitor| {
                    visitor.with_implicit_type_parameters(|visitor| {
                        visitor.visit(&ty);
                        visitor.edge(&ty, &node, "type");

                        for constraint in &constraints {
                            visitor.visit(constraint);
                            visitor.edge(constraint, &node, "constraint");
                        }
                    });

                    visitor.constraint(GroupConstraint::new(node, ty));
                }
            });

            visitor.pop_scope();

            visitor.edge(&self.ty, node, "type");
            visitor.constraint(GroupConstraint::new(node.clone(), self.ty.clone()));

            let definition = ConstantDefinition {
                name: self.name.clone(),
                node: node.clone(),
                comments: self.comments.clone(),
                attributes: ConstantAttributes::from_attributes(visitor.db, &self.attributes),
                value: None,
            };

            visitor.define(&definition.name, definition.clone());

            let node = node.clone();
            visitor.after_all_expressions(move |visitor| {
                let Some(Defined(definition)) = visitor.db.get(&node) else {
                    return;
                };

                if let Definition::Constant(definition) = definition
                    && definition.value.is_none()
                {
                    visitor.db.insert(&node, MissingConstantValue);
                }
            });

            definition
        });
    }
}

impl Codegen for ConstantDefinitionNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        Ok(())
    }
}
