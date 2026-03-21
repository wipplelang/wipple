use crate::{
    codegen::{Codegen, CodegenCtx, CodegenResult, ir},
    database::{Db, Fact, HiddenNode, Node, NodeRef, Render},
    nodes::{Matching, Temporaries, parse_atomic_pattern, visit_pattern},
    syntax::{ParseError, Parser, parse_constructor_name},
    typecheck::{
        Instantation, InstantiateConstraint, Replacements, Substitutions, TypeConstraint, Typed,
    },
    visit::{
        Definition, MarkerConstructorDefinition, MatchPathSegment, VariantConstructorDefinition,
        Visit, Visitor,
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
            visit_pattern(node, visitor, None);
            return;
        };

        match definition {
            ConstructorDefinition::Marker(definition) => {
                visit_pattern(node, visitor, Some(MatchPathSegment::Match));

                for element in &self.elements {
                    visitor.visit_matching(element, None);
                    visitor.edge(element, node, "element");
                    visitor.insert(element, ExtraElement);
                }

                visitor.constraint(InstantiateConstraint::new(Instantation {
                    source_node: node.clone(),
                    definition: definition.node.clone(),
                    substitutions: Substitutions::new(),
                    replacements: Replacements::from_iter([(definition.node, node.clone())]),
                    from_expression: true,
                }));

                visitor.insert(node, ConstructorMatch::Marker);
            }
            ConstructorDefinition::Variant(definition) => {
                visit_pattern(
                    node,
                    visitor,
                    self.elements
                        .is_empty()
                        .then(|| MatchPathSegment::Variant(definition.variant.clone())),
                );

                let element_temporaries = self
                    .elements
                    .iter()
                    .enumerate()
                    .map(|(index, element)| {
                        let temporary = visitor.visit_matching(
                            element,
                            Some(MatchPathSegment::VariantElement(
                                definition.variant.clone(),
                                index,
                                self.elements.len(),
                            )),
                        );

                        visitor.edge(element, node, "element");

                        temporary
                    })
                    .collect::<Vec<_>>();

                if self.elements.is_empty() {
                    visitor.constraint(InstantiateConstraint::new(Instantation {
                        source_node: node.clone(),
                        definition: definition.variant.clone(),
                        substitutions: Substitutions::new(),
                        replacements: Replacements::from_iter([(
                            definition.variant.clone(),
                            node.clone(),
                        )]),
                        from_expression: true,
                    }));
                } else {
                    let span = visitor.span(node);
                    let constructor_node = visitor.node(span, HiddenNode(None));
                    visitor.insert(&constructor_node, Typed::default());

                    visitor.constraint(InstantiateConstraint::new(Instantation {
                        source_node: node.clone(),
                        definition: definition.variant.clone(),
                        substitutions: Substitutions::new(),
                        replacements: Replacements::from_iter([(
                            definition.variant.clone(),
                            constructor_node.clone(),
                        )]),
                        from_expression: true,
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

                visitor.insert(
                    node,
                    Temporaries {
                        has: element_temporaries,
                        inherit: self.elements.clone(),
                    },
                )
            }
        }
    }
}

impl Codegen for ConstructorPatternNode {
    fn codegen(&self, node: &NodeRef, ctx: &mut CodegenCtx<'_>) -> CodegenResult {
        let Matching(matching) = ctx
            .get(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        let constructor_match = ctx
            .get::<ConstructorMatch>(node)
            .ok_or_else(|| anyhow::format_err!("unresolved"))?;

        match constructor_match {
            ConstructorMatch::Marker => {}
            ConstructorMatch::Variant {
                index: variant,
                element_temporaries,
            } => {
                ctx.condition(ir::Condition::EqualToVariant {
                    input: matching.clone(),
                    variant,
                });

                for (index, (element, temporary)) in
                    self.elements.iter().zip(element_temporaries).enumerate()
                {
                    ctx.condition(ir::Condition::Initialize {
                        variable: temporary,
                        node: None,
                        value: ir::Value::VariantElement {
                            input: matching.clone(),
                            variant,
                            index,
                        },
                        mutable: false,
                    });

                    ctx.codegen(element)?;
                }
            }
        }

        Ok(())
    }
}
