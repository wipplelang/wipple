use crate::{
    codegen::Codegen,
    database::{Node, NodeRef},
    nodes::{parse_type, visit_type},
    syntax::{ParseError, Parser, TokenKind, parse_type_parameter_name},
    typecheck::{GroupConstraint, InferredParameter, TypeConstraint},
    visit::{Definition, TypeParameterDefinition, TypeParameters, Visit, Visitor},
};

#[derive(Debug)]
pub struct TypeParameterNode {
    pub name: String,
    pub infer: bool,
    pub value: Option<NodeRef>,
}

impl Node for TypeParameterNode {}

pub fn parse_type_parameter(parser: &mut Parser<'_>) -> Result<TypeParameterNode, ParseError> {
    if let Some(node) = parser.parse_optional(parse_named_type_parameter)? {
        return Ok(node);
    }

    if let Some(node) = parser.parse_optional(parse_infer_type_parameter)? {
        return Ok(node);
    }

    Err(parser.error("Expected type parameter"))
}

pub fn parse_annotated_type_parameter(
    parser: &mut Parser<'_>,
) -> Result<TypeParameterNode, ParseError> {
    let name = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let value = parse_type(parser)?;

    Ok(TypeParameterNode {
        name,
        infer: false,
        value: Some(value),
    })
}

pub fn parse_annotated_type_parameter_node(parser: &mut Parser<'_>) -> Result<NodeRef, ParseError> {
    let span = parser.spanned();
    let node = parse_annotated_type_parameter(parser)?;
    let span = span(parser);
    Ok(parser.register(span, node))
}

pub fn parse_named_type_parameter(
    parser: &mut Parser<'_>,
) -> Result<TypeParameterNode, ParseError> {
    let name = parse_type_parameter_name(parser)?;

    Ok(TypeParameterNode {
        name,
        infer: false,
        value: None,
    })
}

pub fn parse_infer_type_parameter(
    parser: &mut Parser<'_>,
) -> Result<TypeParameterNode, ParseError> {
    parser.token_with_reason(TokenKind::LeftParenthesis, "between these parentheses")?;
    parser.token(TokenKind::InferKeyword)?;
    let name = parse_type_parameter_name(parser)?;
    parser.token(TokenKind::RightParenthesis)?;

    Ok(TypeParameterNode {
        name,
        infer: true,
        value: None,
    })
}

pub fn parse_type_parameters(parser: &mut Parser<'_>) -> Result<Vec<NodeRef>, ParseError> {
    let parameters = parser
        .parse_many(
            0,
            |parser| parser.node(parse_type_parameter),
            |parser| parser.parse_nothing(),
        )?
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    if !parameters.is_empty() {
        parser.token(TokenKind::TypeFunctionOperator)?;
        parser.consume_line_breaks();
    }

    Ok(parameters)
}

impl Visit for TypeParameterNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_type(node, visitor);

        let existing = visitor.resolve(&self.name, node, |definition| match definition {
            Definition::TypeParameter(definition) => Some(definition.clone()),
            _ => None,
        });

        if let Some(existing) = existing {
            visitor.constraint(GroupConstraint::new(node.clone(), existing.node));
        } else {
            let definition = TypeParameterDefinition {
                name: self.name.clone(),
                node: node.clone(),
            };

            visitor.define(&definition.name, definition.clone());

            if let Some(value) = &self.value {
                visitor.visit(value);
                visitor.edge(value, node, "value");

                let mut constraint = GroupConstraint::new(node.clone(), value.clone());
                constraint.info_mut().active = false; // wait until instantiated
                visitor.constraint(constraint);
            }

            if self
                .value
                .as_ref()
                .and_then(|value| value.downcast_ref::<TypeParameterNode>())
                .is_some()
            {
                // Allow aliases to type parameters
            } else {
                visitor.constraint(TypeConstraint::new(
                    node.clone(),
                    visitor.parameter_type(node.clone()),
                ));
            }

            if self.infer {
                visitor.insert(node, InferredParameter);
            }

            // Update the `Resolved` fact
            visitor.resolve(&self.name, node, |definition| match definition {
                Definition::TypeParameter(_) => Some(()),
                _ => None,
            });

            let definition_node = visitor.current_definition().node.clone();

            visitor.with_fact(&definition_node, |TypeParameters(parameters)| {
                parameters.push(node.clone())
            });
        }
    }
}

impl Codegen for TypeParameterNode {}
