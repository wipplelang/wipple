use crate::{
    codegen::{Codegen, CodegenCtx, CodegenError},
    database::{Db, Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{HasTemporaries, InheritTemporaries, Matching, parse_atomic_pattern, visit_pattern},
    syntax::{ParseError, Parser, parse_constructor_name},
    typecheck::{
        Instantation, InstantiateConstraint, Replacements, Substitutions, TypeConstraint, Typed,
    },
    visit::{
        Definition, MarkerConstructorDefinition, VariantConstructorDefinition, Visit, Visitor,
    },
};

#[derive(Debug, Clone)]
pub struct ExtraElement;

impl Fact for ExtraElement {}

impl Render for ExtraElement {
    fn write(&self, w: &mut dyn std::fmt::Write, _db: &Db) -> std::fmt::Result {
        write!(w, "is extra element")
    }
}

#[derive(Debug, Clone)]
pub enum ConstructorMatch {
    Marker,
    Variant {
        index: usize,
        element_temporaries: Vec<NodeRef>,
    },
}

impl Fact for ConstructorMatch {}

impl Render for ConstructorMatch {}

#[derive(Debug)]
pub struct ConstructorPatternNode {
    pub constructor: String,
    pub elements: Vec<NodeRef>,
}

impl Node for ConstructorPatternNode {}

pub fn parse_parameterized_constructor_pattern(
    parser: &mut Parser<'_>,
) -> Result<ConstructorPatternNode, ParseError> {
    let constructor = parse_constructor_name(parser)?;

    let elements = parser
        .parse_optional(|parser| {
            parser.parse_many(1, parse_atomic_pattern, |parser| parser.parse_nothing())
        })?
        .unwrap_or_default()
        .into_iter()
        .map(|(node, _)| node)
        .collect::<Vec<_>>();

    Ok(ConstructorPatternNode {
        constructor,
        elements,
    })
}

pub fn parse_constructor_pattern(
    parser: &mut Parser<'_>,
) -> Result<ConstructorPatternNode, ParseError> {
    let constructor = parse_constructor_name(parser)?;

    Ok(ConstructorPatternNode {
        constructor,
        elements: Vec::new(),
    })
}

#[derive(Debug, Clone)]
enum ConstructorDefinition {
    Marker(MarkerConstructorDefinition),
    Variant(VariantConstructorDefinition),
}

impl Visit for ConstructorPatternNode {
    fn visit(&self, node: &NodeRef, visitor: &mut Visitor<'_>) {
        visit_pattern(node, visitor);

        let Some(definition) =
            visitor.resolve(&self.constructor, node, |definition| match definition {
                Definition::MarkerConstructor(definition) => {
                    Some(ConstructorDefinition::Marker(definition.clone()))
                }
                Definition::VariantConstructor(definition) => {
                    Some(ConstructorDefinition::Variant(definition.clone()))
                }
                _ => None,
            })
        else {
            return;
        };

        match definition {
            ConstructorDefinition::Marker(definition) => {
                for element in &self.elements {
                    visitor.visit_matching(element);
                    visitor.edge(element, node, "element");
                    visitor.insert(element, ExtraElement);
                }

                visitor.constraint(InstantiateConstraint::new(Instantation {
                    source_node: node.clone(),
                    definition: definition.node.clone(),
                    substitutions: Substitutions::new(),
                    replacements: Replacements::from_iter([(definition.node, node.clone())]),

                }));

                visitor.insert(node, ConstructorMatch::Marker);
            }
            ConstructorDefinition::Variant(definition) => {
                let element_temporaries = self
                    .elements
                    .iter()
                    .map(|element| {
                        let temporary = visitor.visit_matching(element);
                        visitor.edge(element, node, "element");
                        temporary
                    })
                    .collect::<Vec<_>>();

                if self.elements.is_empty() {
                    visitor.constraint(InstantiateConstraint::new(Instantation {
                        source_node: node.clone(),
                        definition: definition.node.clone(),
                        substitutions: Substitutions::new(),
                        replacements: Replacements::from_iter([(definition.node, node.clone())]),

                    }));
                } else {
                    let span = visitor.span(node);
                    let constructor_node = visitor.node(span, HiddenNode(None));
                    visitor.insert(&constructor_node, Typed::default());

                    visitor.constraint(InstantiateConstraint::new(Instantation {
                        source_node: node.clone(),
                        definition: definition.node.clone(),
                        substitutions: Substitutions::new(),
                        replacements: Replacements::from_iter([(
                            definition.node,
                            constructor_node.clone(),
                        )]),

                    }));

                    visitor.constraint(TypeConstraint::new(
                        constructor_node,
                        visitor.function_type(self.elements.iter().cloned(), node.clone()),
                    ));
                }

                visitor.insert(
                    node,
                    ConstructorMatch::Variant {
                        index: definition.index,
                        element_temporaries: element_temporaries.clone(),
                    },
                );

                visitor.insert(node, HasTemporaries(element_temporaries));
                visitor.insert(node, InheritTemporaries(self.elements.clone()));
            }
        }
    }
}

impl Codegen for ConstructorPatternNode {
    fn codegen(&self, ctx: &mut CodegenCtx<'_>) -> Result<(), CodegenError> {
        let Matching(matching) = ctx
            .db
            .get::<Matching>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        let constructor_match = ctx
            .db
            .get::<ConstructorMatch>(ctx.current_node())
            .ok_or_else(|| ctx.error())?;

        match constructor_match {
            ConstructorMatch::Marker => {
                // No code needed
                Ok(())
            }
            ConstructorMatch::Variant {
                index,
                element_temporaries,
            } => {
                ctx.write_string(" && (");
                ctx.write_node(&matching);
                ctx.write_string("[__wipple_variant] === ");
                ctx.write_string(index.to_string());
                ctx.write_string(")");

                for (index, (element, temporary)) in
                    self.elements.iter().zip(element_temporaries).enumerate()
                {
                    ctx.write_string(" && ((");
                    ctx.write_node(&temporary);
                    ctx.write_string(" = ");
                    ctx.write_node(&matching);
                    ctx.write_string("[");
                    ctx.write_string(index.to_string());
                    ctx.write_string("]) || true)");

                    ctx.write(element)?;
                }

                Ok(())
            }
        }
    }
}
