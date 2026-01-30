use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, HiddenNode, Node, NodeRef, Render, Span},
    nodes::{
        parse_atomic_type, parse_attributes, parse_comments, parse_type, parse_type_parameters,
    },
    syntax::{
        ParseError, Parser, TokenKind, parse_constructor_name, parse_type_name, parse_variable_name,
    },
    typecheck::{GroupConstraint, TypeConstraint, Typed},
    visit::{
        MarkerConstructorDefinition, StructureConstructorDefinition, TypeAttributes,
        TypeDefinition, VariantConstructorDefinition, Visit, Visitor,
    },
};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone)]
pub struct DuplicateFieldDefinition;

impl Fact for DuplicateFieldDefinition {}

impl Render for DuplicateFieldDefinition {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is duplicate field definition")
    }
}

#[derive(Debug, Clone)]
pub struct DuplicateVariantDefinition;

impl Fact for DuplicateVariantDefinition {}

impl Render for DuplicateVariantDefinition {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is duplicate variant definition")
    }
}

#[derive(Debug, Clone)]
pub enum TypeRepresentation {
    Structure(StructureTypeRepresentation),
    Enumeration(EnumerationTypeRepresentation),
    Marker(MarkerTypeRepresentation),
}

#[derive(Debug, Clone)]
pub struct StructureTypeRepresentation {
    pub fields: Vec<FieldDefinition>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FieldDefinition {
    pub name: String,
    pub ty: NodeRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumerationTypeRepresentation {
    pub variants: Vec<VariantDefinition>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct VariantDefinition {
    pub name: String,
    pub elements: Vec<NodeRef>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct MarkerTypeRepresentation {
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeDefinitionNode {
    pub comments: Vec<String>,
    pub attributes: Vec<NodeRef>,
    pub name: String,
    pub parameters: Vec<NodeRef>,
    pub representation: TypeRepresentation,
}

impl Node for TypeDefinitionNode {}

pub fn parse_type_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<TypeDefinitionNode, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let name = parse_type_name(parser)?;

    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();

    let parameters = parse_type_parameters(parser)?;
    let representation = parse_type_representation(parser)?;

    Ok(TypeDefinitionNode {
        comments,
        attributes,
        name,
        parameters,
        representation,
    })
}

pub fn parse_type_representation(
    parser: &mut Parser<'_>,
) -> Result<TypeRepresentation, ParseError> {
    parser.parse_cached(|parser| {
        if let Some(representation) = parser.parse_optional(parse_structure_type_representation)? {
            return Ok(TypeRepresentation::Structure(representation));
        }

        if let Some(representation) =
            parser.parse_optional(parse_enumeration_type_representation)?
        {
            return Ok(TypeRepresentation::Enumeration(representation));
        }

        if let Some(representation) = parser.parse_optional(parse_marker_type_representation)? {
            return Ok(TypeRepresentation::Marker(representation));
        }

        Err(parser.error("Expected type representation"))
    })
}

pub fn parse_structure_type_representation(
    parser: &mut Parser<'_>,
) -> Result<StructureTypeRepresentation, ParseError> {
    let span = parser.spanned();

    parser.token(TokenKind::TypeKeyword)?;
    parser.token(TokenKind::LeftBrace)?;

    let fields = parser.parse_lines(1, true, parse_field_definition)?;

    parser.token(TokenKind::RightBrace)?;

    Ok(StructureTypeRepresentation {
        fields,
        span: span(parser),
    })
}

pub fn parse_field_definition(parser: &mut Parser<'_>) -> Result<FieldDefinition, ParseError> {
    let span = parser.spanned();

    let name = parse_variable_name(parser)?;
    parser.token(TokenKind::AnnotateOperator)?;
    parser.commit("in this type annotation");
    parser.consume_line_breaks();
    let ty = parse_type(parser)?;

    Ok(FieldDefinition {
        name,
        ty,
        span: span(parser),
    })
}

pub fn parse_enumeration_type_representation(
    parser: &mut Parser<'_>,
) -> Result<EnumerationTypeRepresentation, ParseError> {
    let span = parser.spanned();

    parser.token(TokenKind::TypeKeyword)?;
    parser.token(TokenKind::LeftBrace)?;

    let variants = parser.parse_lines(1, true, parse_variant_definition)?;

    parser.token(TokenKind::RightBrace)?;

    Ok(EnumerationTypeRepresentation {
        variants,
        span: span(parser),
    })
}

pub fn parse_marker_type_representation(
    parser: &mut Parser<'_>,
) -> Result<MarkerTypeRepresentation, ParseError> {
    let span = parser.spanned();

    parser.token(TokenKind::TypeKeyword)?;
    parser.commit("in this type definition");

    Ok(MarkerTypeRepresentation { span: span(parser) })
}

pub fn parse_variant_definition(parser: &mut Parser<'_>) -> Result<VariantDefinition, ParseError> {
    let span = parser.spanned();

    let name = parse_constructor_name(parser)?;

    let elements = if let Some(elements) = parser.parse_optional(|parser| {
        parser.parse_many(1, parse_atomic_type, |parser| parser.parse_nothing())
    })? {
        elements.into_iter().map(|(node, _)| node).collect()
    } else {
        Vec::new()
    };

    Ok(VariantDefinition {
        name,
        elements,
        span: span(parser),
    })
}

impl Visit for TypeDefinitionNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visitor.defining(node, |visitor| {
            visitor.push_scope();

            visitor.with_implicit_type_parameters(|visitor| {
                for parameter in &self.parameters {
                    visitor.visit(parameter);
                }
            });

            visitor.constraint(TypeConstraint::new(
                node.clone(),
                visitor.named_type(node.clone(), self.parameters.iter().cloned()),
            ));

            let type_constraints = visitor.current_definition().constraints(visitor.db);

            let definition = TypeDefinition {
                name: self.name.clone(),
                node: node.clone(),
                comments: self.comments.clone(),
                attributes: TypeAttributes::from_attributes(visitor.db, &self.attributes),
                parameters: self.parameters.clone(),
            };

            if !definition.attributes.intrinsic {
                visitor.after_type_definitions({
                    let representation = self.representation.clone();
                    let definition = definition.clone();
                    let node = node.clone();
                    move |visitor| match representation {
                        TypeRepresentation::Marker(_) => {
                            visitor.pop_scope();

                            visitor.define(
                                &definition.name,
                                MarkerConstructorDefinition {
                                    name: definition.name.clone(),
                                    node: definition.node.clone(),
                                    comments: definition.comments.clone(),
                                },
                            );
                        }
                        TypeRepresentation::Structure(representation) => {
                            let mut fields = BTreeMap::new();
                            for field in representation.fields {
                                visitor.visit(&field.ty);
                                visitor.edge(&field.ty, &node, "field");

                                if fields.contains_key(&field.name) {
                                    visitor.insert(&node, DuplicateFieldDefinition);
                                    continue;
                                }

                                fields.insert(field.name, field.ty);
                            }

                            visitor.pop_scope();

                            visitor.define(
                                &definition.name,
                                StructureConstructorDefinition {
                                    name: definition.name.clone(),
                                    node: definition.node.clone(),
                                    comments: definition.comments.clone(),
                                    fields,
                                },
                            );
                        }
                        TypeRepresentation::Enumeration(representation) => {
                            let mut variant_definitions = HashMap::new();
                            for (index, variant) in representation.variants.into_iter().enumerate()
                            {
                                if variant_definitions.contains_key(&variant.name) {
                                    visitor.insert(&node, DuplicateVariantDefinition);
                                    continue;
                                }

                                let variant_node = visitor.node(
                                    variant.span.clone(),
                                    VariantNode {
                                        index,
                                        variant: variant.clone(),
                                    },
                                );

                                visitor.insert(&variant_node, Typed::default());

                                let constructor_definition =
                                    visitor.defining(&variant_node.clone(), |visitor| {
                                        for constraint in &type_constraints {
                                            visitor.constraint(constraint.clone());
                                        }

                                        for element in &variant.elements {
                                            visitor.visit(element);
                                            visitor.edge(element, &variant_node, "element");
                                        }

                                        if variant.elements.is_empty() {
                                            visitor.constraint(GroupConstraint::new(
                                                variant_node.clone(),
                                                node.clone(),
                                            ));
                                        } else {
                                            visitor.constraint(TypeConstraint::new(
                                                variant_node.clone(),
                                                visitor.function_type(
                                                    variant.elements.iter().cloned(),
                                                    node.clone(),
                                                ),
                                            ));
                                        }

                                        VariantConstructorDefinition {
                                            name: variant.name.clone(),
                                            node: variant_node,
                                            index,
                                        }
                                    });

                                variant_definitions
                                    .insert(variant.name.clone(), constructor_definition);
                            }

                            visitor.pop_scope();

                            for (name, constructor_definition) in variant_definitions {
                                visitor.define(&name, constructor_definition);
                            }
                        }
                    }
                });
            }

            visitor.pop_scope();

            visitor.define(&definition.name, definition.clone());

            definition
        });
    }
}

impl Codegen for TypeDefinitionNode {
    fn codegen(&self, _codegen: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        // Handled specially in `Codegen`
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct VariantNode {
    pub index: usize,
    pub variant: VariantDefinition,
}

impl Node for VariantNode {}

impl Visit for VariantNode {
    fn visit(&self, _node: &NodeRef, _visitor: &mut Visitor<'_>) {}
}

impl Codegen for VariantNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let element_temporaries = self
            .variant
            .elements
            .iter()
            .map(|element| {
                let span = ctx.db.span(element);
                ctx.db.node(span, HiddenNode(None))
            })
            .collect::<Vec<_>>();

        if element_temporaries.is_empty() {
            ctx.write_string(format!("__wipple_variant({}, [])", self.index));
        } else {
            ctx.write_string("(async (");
            for temporary in &element_temporaries {
                ctx.write_node(temporary);
                ctx.write_string(", ");
            }
            ctx.write_string(") => {");
            ctx.write_line();

            ctx.write_string(format!("return __wipple_variant({}, [", self.index));
            for temporary in &element_temporaries {
                ctx.write_node(temporary);
                ctx.write_string(", ");
            }
            ctx.write_string("]);");
            ctx.write_line();

            ctx.write_string("})");
        }

        Ok(())
    }
}
