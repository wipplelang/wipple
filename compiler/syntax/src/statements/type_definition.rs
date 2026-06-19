use crate::{
    attributes::attribute::parse_attributes,
    statements::{parse_comments, visit_statement},
    types::{parse_atomic_type, parse_type, type_parameter::parse_type_parameters},
};

use serde::{Deserialize, Serialize};
use wipple_core::{
    ast::AstKey,
    db::{Db, Fact, Node},
    facts::Syntax,
    render::Render,
    span::{Span, Str},
    typecheck::{
        constraints::ty_constraint::TyConstraint,
        groups::Typed,
        ty::{ConstructedTy, Ty},
    },
    visit::{
        DefinitionConstraints, Visit, Visitor,
        definitions::{
            self, Definition, MarkerConstructorDefinition, StructureConstructorDefinition,
            TypeDefinitionAttributes, VariantConstructorDefinition,
        },
        exhaustiveness::{EnumerationVariants, MarkerType, StructureFields},
    },
};
use wipple_parse::{
    lexer::TokenKind,
    names::{parse_constructor_name, parse_type_name, parse_variable_name},
    parse_alt,
    parser::{ParseError, Parser},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuplicateFieldDefinition;

#[typetag::serde]
impl Fact for DuplicateFieldDefinition {}

impl Render for DuplicateFieldDefinition {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuplicateVariantDefinition;

#[typetag::serde]
impl Fact for DuplicateVariantDefinition {}

impl Render for DuplicateVariantDefinition {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDefinition {
    pub span: Span,
    pub comments: Vec<Str>,
    pub attributes: Vec<AstKey>,
    pub name: Str,
    pub parameters: Vec<AstKey>,
    pub representation: AstKey,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureTypeRepresentation {
    pub span: Span,
    pub fields: Vec<FieldDefinition>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDefinition {
    pub span: Span,
    pub name: Str,
    pub ty: AstKey,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumerationTypeRepresentation {
    pub span: Span,
    pub variants: Vec<VariantDefinition>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarkerTypeRepresentation {
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariantDefinition {
    pub span: Span,
    pub name: Str,
    pub elements: Vec<AstKey>,
}

pub fn parse_type_definition_statement(
    parser: &mut Parser<'_>,
) -> Result<TypeDefinition, ParseError> {
    let comments = parse_comments(parser)?;
    let attributes = parse_attributes(parser)?;
    let span = parser.spanned();
    let name = parse_type_name(parser)?;
    parser.token(TokenKind::AssignOperator)?;
    parser.consume_line_breaks();
    let parameters = parse_type_parameters(parser)?;
    parser.token(TokenKind::TypeKeyword)?;
    let representation = parse_type_representation(parser)?;
    Ok(TypeDefinition {
        span: span(parser),
        comments,
        attributes,
        name,
        parameters,
        representation,
    })
}

pub fn parse_type_representation(parser: &mut Parser<'_>) -> Result<AstKey, ParseError> {
    parse_alt!(parser, {
        parse_structure_type_representation as value => parser.in_ast(value),
        parse_enumeration_type_representation as value => parser.in_ast(value),
        parse_marker_type_representation as value => parser.in_ast(value),
        _ => "Expected type representation",
    })
}

pub fn parse_structure_type_representation(
    parser: &mut Parser<'_>,
) -> Result<StructureTypeRepresentation, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LeftBrace)?;
    let fields = parser.parse_lines(1, true, parse_field_definition)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(StructureTypeRepresentation {
        span: span(parser),
        fields,
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
        span: span(parser),
        name,
        ty,
    })
}

pub fn parse_enumeration_type_representation(
    parser: &mut Parser<'_>,
) -> Result<EnumerationTypeRepresentation, ParseError> {
    let span = parser.spanned();
    parser.token(TokenKind::LeftBrace)?;
    let variants = parser.parse_lines(1, true, parse_variant_definition)?;
    parser.token(TokenKind::RightBrace)?;
    Ok(EnumerationTypeRepresentation {
        span: span(parser),
        variants,
    })
}

pub fn parse_marker_type_representation(
    parser: &mut Parser<'_>,
) -> Result<MarkerTypeRepresentation, ParseError> {
    let span = parser.spanned();
    parser.commit("in this type definition");
    Ok(MarkerTypeRepresentation { span: span(parser) })
}

pub fn parse_variant_definition(parser: &mut Parser<'_>) -> Result<VariantDefinition, ParseError> {
    let span = parser.spanned();
    let name = parse_constructor_name(parser)?;
    let elements = parser.parse_many(0, parse_atomic_type)?;
    Ok(VariantDefinition {
        span: span(parser),
        name,
        elements,
    })
}

#[typetag::serde]
impl Visit for TypeDefinition {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
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
            .map(|attribute| visitor.visit(db, &attribute.clone()))
            .collect::<Vec<_>>();

        let mut definitions: Vec<(Node, Box<dyn Definition>)> = vec![(
            node,
            Box::new(definitions::TypeDefinition {
                name: self.name.clone(),
                comments: self.comments.clone(),
                attributes: TypeDefinitionAttributes::parse(db, &attributes),
                parameters: self.parameters.iter().map(|_| db.node()).collect(),
            }),
        )];

        if db
            .ast(&self.representation)
            .downcast_ref::<MarkerTypeRepresentation>()
            .is_some()
        {
            let constructor = db.node();

            definitions.push((
                constructor,
                Box::new(definitions::MarkerConstructorDefinition {
                    name: self.name.clone(),
                    comments: self.comments.clone(),
                }),
            ));

            db.insert(node, MarkerType { constructor });
        } else if let Some(representation) = db
            .ast(&self.representation)
            .downcast_ref::<StructureTypeRepresentation>()
            .cloned()
        {
            let constructor = db.node();

            let fields = representation
                .fields
                .iter()
                .map(|_| db.node())
                .collect::<Vec<_>>();

            definitions.push((
                constructor,
                Box::new(definitions::StructureConstructorDefinition {
                    name: self.name.clone(),
                    comments: self.comments.clone(),
                    fields: fields
                        .iter()
                        .zip(&representation.fields)
                        .map(|(&node, field)| (field.name.clone(), node))
                        .collect(),
                }),
            ));

            db.insert(
                node,
                StructureFields {
                    constructor,
                    fields,
                },
            );
        } else if let Some(representation) = db
            .ast(&self.representation)
            .downcast_ref::<EnumerationTypeRepresentation>()
            .cloned()
        {
            let constructors = representation
                .variants
                .iter()
                .enumerate()
                .map(|(index, variant)| {
                    let constructor = db.node();

                    let elements = variant
                        .elements
                        .iter()
                        .map(|_| db.node())
                        .collect::<Vec<_>>();

                    definitions.push((
                        constructor,
                        Box::new(definitions::VariantConstructorDefinition {
                            name: variant.name.clone(),
                            type_definition: node,
                            index,
                            elements: elements.clone(),
                        }),
                    ));

                    (constructor, elements)
                })
                .collect::<Vec<_>>();

            db.insert(node, EnumerationVariants(constructors));
        }

        definitions
    }

    fn visit(self: Box<Self>, db: &mut Db, node: Node, visitor: &mut Visitor) {
        visit_statement(db, node, visitor);

        visitor.within_definition::<definitions::TypeDefinition>(
            db,
            node,
            |db, visitor, definition| {
                visitor.push_scope(db, node);

                let parameters = self.parameters;
                visitor.with_definition_flag(
                    |d| &mut d.implicit_type_parameters,
                    |visitor| {
                        for (&parameter_node, parameter) in
                            definition.parameters.iter().zip(parameters)
                        {
                            visitor.visit_as(db, &parameter, parameter_node)
                        }
                    },
                );

                visitor.constraint(
                    db,
                    TyConstraint::new(
                        node,
                        Ty::Constructed(ConstructedTy::named(node, definition.parameters.clone())),
                    ),
                );

                let type_constraints = visitor
                    .current_definition
                    .as_ref()
                    .unwrap()
                    .constraints(db)
                    .to_vec();

                if !definition.attributes.intrinsic {
                    if db
                        .ast(&self.representation)
                        .downcast_ref::<MarkerTypeRepresentation>()
                        .is_some()
                    {
                        let MarkerType { constructor } = db.get(node).cloned().unwrap();

                        visitor.within_definition::<MarkerConstructorDefinition>(
                            db,
                            constructor,
                            |db, visitor, _| {
                                db.insert(constructor, Typed::default());

                                db.get_mut_or_default::<DefinitionConstraints>(constructor)
                                    .0
                                    .extend(type_constraints.clone());

                                visitor
                                    .constraint(db, TyConstraint::new(constructor, Ty::Node(node)));

                                if let Some(Syntax(syntax)) = db.get(node) {
                                    db.insert(constructor, Syntax(syntax.clone()));
                                }
                            },
                        );

                        visitor.pop_scope(db);
                    } else if let Some(representation) = db
                        .ast(&self.representation)
                        .downcast_ref::<StructureTypeRepresentation>()
                        .cloned()
                    {
                        let StructureFields { constructor, .. } = db.get(node).cloned().unwrap();

                        visitor.within_definition::<StructureConstructorDefinition>(
                            db,
                            constructor,
                            |db, visitor, definition| {
                                let mut fields = Vec::new();

                                for (&(_, field_node), field) in
                                    definition.fields.iter().zip(&representation.fields)
                                {
                                    visitor.visit_as(db, &field.ty.clone(), field_node);
                                    db.graph.edge(field_node, node, "field");

                                    if fields.iter().any(|(name, _)| name == &field.name) {
                                        db.insert(node, DuplicateFieldDefinition);
                                        continue;
                                    }

                                    fields.push((field.name.clone(), field_node));
                                }

                                db.insert(constructor, Typed::default());

                                db.get_mut_or_default::<DefinitionConstraints>(constructor)
                                    .0
                                    .extend(type_constraints.clone());

                                visitor
                                    .constraint(db, TyConstraint::new(constructor, Ty::Node(node)));

                                if let Some(Syntax(syntax)) = db.get(node) {
                                    db.insert(constructor, Syntax(syntax.clone()));
                                }
                            },
                        );

                        visitor.pop_scope(db);
                    } else if let Some(representation) = db
                        .ast(&self.representation)
                        .downcast_ref::<EnumerationTypeRepresentation>()
                        .cloned()
                    {
                        let EnumerationVariants(variants) = db.get(node).cloned().unwrap();

                        let mut constructors = Vec::<VariantConstructorDefinition>::new();

                        for (&(constructor, ref elements), variant) in
                            variants.iter().zip(&representation.variants)
                        {
                            visitor.within_definition::<VariantConstructorDefinition>(
                                db,
                                constructor,
                                |db, visitor, definition| {
                                    if constructors
                                        .iter()
                                        .any(|constructor| constructor.name == variant.name)
                                    {
                                        db.insert(node, DuplicateVariantDefinition);
                                        return;
                                    }

                                    let variant_definition = visitor.in_ast(
                                        db,
                                        Box::new(VariantDefinition {
                                            span: variant.span.clone(),
                                            name: variant.name.clone(),
                                            elements: variant.elements.clone(),
                                        }),
                                    );

                                    db.insert(constructor, Syntax(variant_definition));

                                    visitor.within_definition::<VariantConstructorDefinition>(
                                        db,
                                        constructor,
                                        |db, visitor, definition| {
                                            for (&element_node, element) in
                                                definition.elements.iter().zip(&variant.elements)
                                            {
                                                visitor.visit_as(
                                                    db,
                                                    &element.clone(),
                                                    element_node,
                                                );
                                                db.graph.edge(element_node, constructor, "element");
                                            }

                                            db.insert(constructor, Typed::default());

                                            db.get_mut_or_default::<DefinitionConstraints>(
                                                constructor,
                                            )
                                            .0
                                            .extend(type_constraints.clone());

                                            if definition.elements.is_empty() {
                                                visitor.constraint(
                                                    db,
                                                    TyConstraint::new(constructor, Ty::Node(node)),
                                                );
                                            } else {
                                                visitor.constraint(
                                                    db,
                                                    TyConstraint::new(
                                                        constructor,
                                                        Ty::Constructed(ConstructedTy::function(
                                                            elements.clone(),
                                                            node,
                                                        )),
                                                    ),
                                                );
                                            }
                                        },
                                    );

                                    constructors.push(definition.clone());
                                },
                            );
                        }

                        visitor.pop_scope(db);
                    } else {
                        visitor.pop_scope(db);
                    }
                } else {
                    visitor.pop_scope(db);
                }
            },
        );
    }
}

#[typetag::serde]
impl Visit for StructureTypeRepresentation {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for FieldDefinition {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for EnumerationTypeRepresentation {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for MarkerTypeRepresentation {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}

#[typetag::serde]
impl Visit for VariantDefinition {
    fn span<'a>(&'a self, _db: &'a Db) -> &'a Span {
        &self.span
    }
}
